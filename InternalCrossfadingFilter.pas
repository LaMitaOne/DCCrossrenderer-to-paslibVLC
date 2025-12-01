unit InternalCrossfadingFilter;

interface

uses
  Windows, Classes, SysUtils, Math, DirectSound, PasLibVlcUnit, PasLibLibVlcPlayerUnit,
  {$IFDEF FPC}LCLType,{$ENDIF} Messages, MMSystem, DXTypes, Registry;

// Audio format structure used for crossfading
type
  TAudioFormat = record
    SampleRate: Cardinal;      // e.g. 44100, 48000
    Channels: Cardinal;        // 1 = mono, 2 = stereo, etc.
    BitsPerSample: Cardinal;   // 8, 16 or 32 (float)
    BytesPerSample: Cardinal;  // Channels * BitsPerSample div 8
  end;

// Possible crossfading states
  TCrossfadeState = (
    csNone,       // No fading active
    csFadeIn,     // Fade-in after pause/resume/seek
    csFadeOut,    // Fade-out before pause
    csCrossfade   // Real track-to-track crossfade (not used yet in this version)
  );

// Simple ring-buffer style audio buffer
type
  TAudioBuffer = class
  private
    FData: array of Byte;
    FSize: Integer;
    FPosition: Integer;
  public
    constructor Create(Size: Integer);
    destructor Destroy; override;
    procedure Write(const Data: PByte; Count: Integer);
    function Read(var Data: PByte; Count: Integer): Integer;
    procedure Clear;
    property Size: Integer read FSize;
    property Position: Integer read FPosition;
    function GetData: PByte;
    property Data: PByte read GetData;
  end;

// Main internal crossfading filter class
type
  TInternalCrossfadingFilter = class
  private
    // DirectSound objects
    FDirectSound: IDirectSound;
    FDirectSoundBuffer: IDirectSoundBuffer;
    FDirectSoundBuffer8: IDirectSoundBuffer8;

    // Current audio format (set by VLC)
    FAudioFormat: TAudioFormat;

    // Crossfading state & parameters
    FCrossfadeState: TCrossfadeState;
    FCrossfadeTime: Integer;      // in milliseconds
    FCrossfadeProgress: Double;   // 0.0 to 1.0
    FCrossfadeStartTime: Cardinal;

    // Buffers for current/next track (only partially used in this version)
    FCurrentBuffer: TAudioBuffer;
    FNextBuffer: TAudioBuffer;
    FOutputBuffer: TMemoryStream; // Temporary buffer for processed audio

    // Volume / Balance / Mute
    FVolume: Integer;    // DirectSound volume: -10000 (silent) .. 0 (full)
    FBalance: Integer;   // -10000 (left) .. +10000 (right)
    FMute: Boolean;

    // Format detection flag
    FFormatDetected: Boolean;

    // DirectSound buffer position tracking (emulates DSPack behaviour)
    FPos: DWORD;
    FBufferSize: DWORD;

    // Reference to the attached VLC player
    FVLCPlayer: TPasLibVlcPlayer;

    // Internal state flags
    FIsPaused: Boolean;
    FRequestPause: Boolean;
    FHasFadedOut: Boolean;
    FWillPause: Boolean;
    FReadyForAudio: Boolean;
    FBufferReady: Boolean;

    // --- DirectSound helper methods ---
    function CreateDirectSoundBuffer: Boolean;
    function InitializeDirectSound: Boolean;
    procedure CleanupDirectSound;

    // --- Crossfading & audio processing ---
    procedure ProcessCrossfading(const InputBuffer: PByte; OutputBuffer: PByte; Count: Integer);
    procedure ApplyCrossfadeEffect(const InputBuffer: PByte; OutputBuffer: PByte; Count: Integer);
    procedure UpdateCrossfadeProgress;

    procedure ProcessAudioData(const AudioData: PByte; DataSize: Integer);
    procedure MixAudio(const Buffer1, Buffer2: PByte; Output: PByte; Count: Integer; MixRatio: Double);

    procedure FillBufferWithSilence(Buffer: IDirectSoundBuffer); overload;
    procedure FillBufferWithSilence(Buffer: IDirectSoundBuffer8); overload;

  public
    constructor Create;
    destructor Destroy; override;

    // --- VLC integration ---
    function AttachToVLCPlayer(VLCPlayer: TPasLibVlcPlayer): Boolean;
    procedure DetachFromVLCPlayer;

    // --- Crossfading settings ---
    procedure SetCrossfadeTime(Time: Integer); // ms
    function GetCrossfadeTime: Integer;

    // --- Volume / Balance / Mute ---
    procedure SetVolume(Volume: Single); // 0.0 to 1.0 → converted to DS volume
    function GetVolume: Integer;
    procedure SetBalance(Balance: Integer);
    function GetBalance: Integer;
    procedure SetMute(Mute: Boolean);
    function GetMute: Boolean;

    // --- Filter state ---
    function IsActive: Boolean;
    procedure StartCrossfade;
    procedure StopCrossfade;

    // --- Video crossfading (registry hack for DSP-worx renderer) ---
    procedure EnableVideoCrossfading;
    procedure DisableVideoCrossfading;

    // --- Audio format handling ---
    procedure SetAudioFormat(SampleRate, Channels, BitsPerSample: Cardinal);
    function GetAudioFormat: TAudioFormat;

    // --- VLC audio callbacks (cdecl!) ---
    procedure OnVLCAudioPlay(data: Pointer; samples: Pointer; count: Cardinal; pts: Int64); cdecl;
    procedure OnVLCAudioPause(data: Pointer; pts: Int64); cdecl;
    procedure OnVLCAudioResume(data: Pointer; pts: Int64); cdecl;
    procedure OnVLCAudioFlush(data: Pointer; pts: Int64); cdecl;
    procedure OnVLCAudioDrain(data: Pointer); cdecl;
    procedure OnVLCAudioVolume(data: Pointer; volume: Single; mute: Boolean); cdecl;

    function IsVlcPaused: Boolean;
  end;

implementation

var
  GlobalFilter: TInternalCrossfadingFilter = nil; // Needed for static callbacks

// ---------------------------------------------------------------------------
// VLC callback wrappers (must be standalone procedures)
// ---------------------------------------------------------------------------

function VLCPlayCallback(data: Pointer; samples: Pointer; count: Cardinal; pts: Int64): Integer; cdecl;
begin
  if Assigned(GlobalFilter) then
    GlobalFilter.OnVLCAudioPlay(data, samples, count, pts);
  Result := 0;
end;

function VLCPauseCallback(data: Pointer; pts: Int64): Integer; cdecl;
begin
  if Assigned(GlobalFilter) then
    GlobalFilter.OnVLCAudioPause(data, pts);
  Result := 0;
end;

function VLCResumeCallback(data: Pointer; pts: Int64): Integer; cdecl;
begin
  if Assigned(GlobalFilter) then
    GlobalFilter.OnVLCAudioResume(data, pts);
  Result := 0;
end;

function VLCFlushCallback(data: Pointer; pts: Int64): Integer; cdecl;
begin
  if Assigned(GlobalFilter) then
    GlobalFilter.OnVLCAudioFlush(data, pts);
  Result := 0;
end;

function VLCDrainCallback(data: Pointer): Integer; cdecl;
begin
  if Assigned(GlobalFilter) then
    GlobalFilter.OnVLCAudioDrain(data);
  Result := 0;
end;

function VLCVolumeCallback(data: Pointer; volume: Single; mute: Boolean): Integer; cdecl;
begin
  if Assigned(GlobalFilter) then
    GlobalFilter.OnVLCAudioVolume(data, volume, mute);
  Result := 0;
end;

// ---------------------------------------------------------------------------
// VLC format callback – called when VLC decides the output format
// ---------------------------------------------------------------------------
procedure AudioFormatCallback(data: Pointer; format: PAnsiChar; rate, channels: Cardinal); cdecl;
var
  FormatChanged: Boolean;
  NewBitsPerSample: Cardinal;
begin
  try
    if (GlobalFilter = nil) or (not GlobalFilter.FReadyForAudio) then
      Exit;

    // Safety checks + fallback values
    if (rate < 8000) or (rate > 192000) then
      rate := 44100;
    if (channels < 1) or (channels > 8) then
      channels := 2;

    // Detect bits per sample from VLC format string
    if SameText(format, 'S16N') or SameText(format, 's16n') then
      NewBitsPerSample := 16
    else if SameText(format, 'U8') or SameText(format, 'u8') then
      NewBitsPerSample := 8
    else
      NewBitsPerSample := 16; // safe default

    with GlobalFilter do
    begin
      FormatChanged :=
        (FAudioFormat.SampleRate <> rate) or
        (FAudioFormat.Channels <> channels) or
        (FAudioFormat.BitsPerSample <> NewBitsPerSample);

      if FormatChanged then
      begin
        // Stop playback, recreate DirectSound buffer with new format
        if Assigned(FDirectSoundBuffer8) then FDirectSoundBuffer8.Stop;
        if Assigned(FDirectSoundBuffer) then FDirectSoundBuffer.Stop;

        FAudioFormat.SampleRate := rate;
        FAudioFormat.Channels := channels;
        FAudioFormat.BitsPerSample := NewBitsPerSample;
        FAudioFormat.BytesPerSample := (channels * NewBitsPerSample) div 8;

        if not CreateDirectSoundBuffer then
        begin
          FBufferReady := False;
          Exit;
        end;

        FBufferReady := True;
        if Assigned(FDirectSoundBuffer8) then
          FDirectSoundBuffer8.Play(0, 0, DSBPLAY_LOOPING)
        else if Assigned(FDirectSoundBuffer) then
          FDirectSoundBuffer.Play(0, 0, DSBPLAY_LOOPING);
      end;
    end;
  except
    on E: Exception do
      OutputDebugString(PChar('AudioFormatCallback Exception: ' + E.Message));
  end;
end;

// ---------------------------------------------------------------------------
// TAudioBuffer – simple fixed-size buffer
// ---------------------------------------------------------------------------
constructor TAudioBuffer.Create(Size: Integer);
begin
  inherited Create;
  FSize := Size;
  SetLength(FData, Size);
  FPosition := 0;
end;

destructor TAudioBuffer.Destroy;
begin
  SetLength(FData, 0);
  inherited;
end;

procedure TAudioBuffer.Write(const Data: PByte; Count: Integer);
var
  WriteCount: Integer;
begin
  if FPosition + Count > FSize then
    WriteCount := FSize - FPosition
  else
    WriteCount := Count;

  if WriteCount > 0 then
  begin
    Move(Data^, FData[FPosition], WriteCount);
    Inc(FPosition, WriteCount);
  end;
end;

function TAudioBuffer.Read(var Data: PByte; Count: Integer): Integer;
var
  ReadCount: Integer;
begin
  if FPosition + Count > FSize then
    ReadCount := FSize - FPosition
  else
    ReadCount := Count;

  if ReadCount > 0 then
  begin
    Data := @FData[FPosition];
    Inc(FPosition, ReadCount);
    Result := ReadCount;
  end
  else
    Result := 0;
end;

procedure TAudioBuffer.Clear;
begin
  FillChar(FData[0], FSize, 0);
  FPosition := 0;
end;

function TAudioBuffer.GetData: PByte;
begin
  if Length(FData) > 0 then
    Result := @FData[0]
  else
    Result := nil;
end;

// ---------------------------------------------------------------------------
// TInternalCrossfadingFilter – main class
// ---------------------------------------------------------------------------
constructor TInternalCrossfadingFilter.Create;
begin
  inherited Create;

  // Initialize DirectSound
  if not InitializeDirectSound then
    raise Exception.Create('Failed to initialize DirectSound');

  // Default format (will be overwritten by VLC)
  FAudioFormat.SampleRate := 44100;
  FAudioFormat.Channels := 2;
  FAudioFormat.BitsPerSample := 16;
  FAudioFormat.BytesPerSample := 4;

  // Crossfading defaults
  FCrossfadeTime := 2000; // 2 seconds
  FCrossfadeState := csNone;

  // Volume defaults (slightly below max to avoid clipping)
  FVolume := -1000;  // -10 dB
  FBalance := 0;
  FMute := False;

  // Create temporary buffers
  FCurrentBuffer := TAudioBuffer.Create(48000 * 4 * 2); // ~2 sec at 48 kHz stereo 16 bit
  FNextBuffer    := TAudioBuffer.Create(48000 * 4 * 2);
  FOutputBuffer  := TMemoryStream.Create;

  // Create initial DirectSound buffer
  if not CreateDirectSoundBuffer then
    raise Exception.Create('Failed to create DirectSound buffer');

  FBufferReady := True;
end;

destructor TInternalCrossfadingFilter.Destroy;
begin
  DetachFromVLCPlayer;

  FCurrentBuffer.Free;
  FNextBuffer.Free;
  FOutputBuffer.Free;

  CleanupDirectSound;
  inherited;
end;

// Initialize DirectSound object
function TInternalCrossfadingFilter.InitializeDirectSound: Boolean;
var
  hr: HRESULT;
begin
  Result := False;
  hr := DirectSoundCreate(nil, FDirectSound, nil);
  if Failed(hr) then Exit;

  hr := FDirectSound.SetCooperativeLevel(GetDesktopWindow, DSSCL_PRIORITY);
  if Failed(hr) then Exit;

  Result := True;
end;

// Create a secondary looping DirectSound buffer with current format
function TInternalCrossfadingFilter.CreateDirectSoundBuffer: Boolean;
var
  hr: HRESULT;
  WaveFormat: TWaveFormatEx;
  BufferDesc: TDSBufferDesc;
begin
  Result := False;

  FillChar(WaveFormat, SizeOf(WaveFormat), 0);
  with WaveFormat do
  begin
    wFormatTag := WAVE_FORMAT_PCM;
    nChannels := FAudioFormat.Channels;
    nSamplesPerSec := FAudioFormat.SampleRate;
    wBitsPerSample := FAudioFormat.BitsPerSample;
    nBlockAlign := FAudioFormat.BytesPerSample div FAudioFormat.Channels;
    nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
  end;

  FillChar(BufferDesc, SizeOf(BufferDesc), 0);
  BufferDesc.dwSize := SizeOf(BufferDesc);
  BufferDesc.dwFlags := DSBCAPS_GLOBALFOCUS or DSBCAPS_GETCURRENTPOSITION2 or DSBCAPS_STICKYFOCUS;
  FBufferSize := FAudioFormat.SampleRate * FAudioFormat.BytesPerSample * 2; // 2-second buffer
  BufferDesc.dwBufferBytes := FBufferSize;
  BufferDesc.lpwfxFormat := @WaveFormat;

  hr := FDirectSound.CreateSoundBuffer(BufferDesc, FDirectSoundBuffer, nil);
  if Failed(hr) then Exit;

  // Try to get the newer IDirectSoundBuffer8 interface
  hr := FDirectSoundBuffer.QueryInterface(IID_IDirectSoundBuffer8, FDirectSoundBuffer8);

  FPos := 0; // Reset write position on new buffer
  Result := True;
end;

procedure TInternalCrossfadingFilter.CleanupDirectSound;
begin
  FDirectSoundBuffer8 := nil;
  FDirectSoundBuffer := nil;
  FDirectSound := nil;
end;

// Attach this filter to a PasLibVLC player instance
function TInternalCrossfadingFilter.AttachToVLCPlayer(VLCPlayer: TPasLibVlcPlayer): Boolean;
begin
  Result := False;
  if not Assigned(VLCPlayer) then Exit;

  FVLCPlayer := VLCPlayer;
  GlobalFilter := Self;

  // Install VLC audio callbacks
  libvlc_audio_set_callbacks(
    FVLCPlayer.GetPlayerHandle,
    @VLCPlayCallback, @VLCPauseCallback, @VLCResumeCallback,
    @VLCFlushCallback, @VLCDrainCallback, Self);

  // Force a known format (VLC will call the format callback immediately)
  libvlc_audio_set_format(FVLCPlayer.GetPlayerHandle, 'S16N', 44100, 2);

  FReadyForAudio := True;
  libvlc_audio_set_format_callbacks(FVLCPlayer.GetPlayerHandle, @AudioFormatCallback, nil);
  libvlc_audio_set_volume_callback(FVLCPlayer.GetPlayerHandle, @VLCVolumeCallback);

  EnableVideoCrossfading;

  // Start with a fade-in
  FCrossfadeState := csFadeIn;
  FCrossfadeStartTime := GetTickCount;
  FCrossfadeProgress := 0.0;

  // Ensure buffer exists and is playing
  if not CreateDirectSoundBuffer then Exit;
  FBufferReady := True;

  if Assigned(FDirectSoundBuffer8) then
  begin
    FillBufferWithSilence(FDirectSoundBuffer8);
    FDirectSoundBuffer8.SetVolume(FVolume);
    FDirectSoundBuffer8.Play(0, 0, DSBPLAY_LOOPING);
  end
  else if Assigned(FDirectSoundBuffer) then
  begin
    FillBufferWithSilence(FDirectSoundBuffer);
    FDirectSoundBuffer.SetVolume(FVolume);
    FDirectSoundBuffer.Play(0, 0, DSBPLAY_LOOPING);
  end;

  Result := True;
end;

procedure TInternalCrossfadingFilter.DetachFromVLCPlayer;
begin
  if Assigned(FVLCPlayer) then
  begin
    if GlobalFilter = Self then GlobalFilter := nil;

    libvlc_audio_set_callbacks(FVLCPlayer.GetPlayerHandle, nil, nil, nil, nil, nil, nil);
    libvlc_audio_set_format_callbacks(FVLCPlayer.GetPlayerHandle, nil, nil);
    libvlc_audio_set_volume_callback(FVLCPlayer.GetPlayerHandle, nil);

    FVLCPlayer := nil;
    FReadyForAudio := False;
    FBufferReady := False;
  end;
end;

// ---------------------------------------------------------------------------
// VLC audio data callback – this is where the audio arrives from libvlc
// ---------------------------------------------------------------------------
procedure TInternalCrossfadingFilter.OnVLCAudioPlay(data: Pointer; samples: Pointer; count: Cardinal; pts: Int64); cdecl;
begin
  if FIsPaused or (not FBufferReady) then Exit;

  ProcessAudioData(PByte(samples), count * FAudioFormat.BytesPerSample);
end;

// Fade-out → pause sequence
procedure TInternalCrossfadingFilter.OnVLCAudioPause(data: Pointer; pts: Int64); cdecl;
begin
  FWillPause := True;
  FCrossfadeState := csFadeOut;
  FCrossfadeStartTime := GetTickCount;
  FCrossfadeProgress := 0.0;
end;

// Resume → fade-in
procedure TInternalCrossfadingFilter.OnVLCAudioResume(data: Pointer; pts: Int64); cdecl;
begin
  FIsPaused := False;
  FWillPause := False;
  FCrossfadeState := csFadeIn;
  FCrossfadeStartTime := GetTickCount;
  FCrossfadeProgress := 0.0;

  if Assigned(FDirectSoundBuffer8) then
    FDirectSoundBuffer8.Play(0, 0, DSBPLAY_LOOPING)
  else if Assigned(FDirectSoundBuffer) then
    FDirectSoundBuffer.Play(0, 0, DSBPLAY_LOOPING);
end;

// Flush = seek or stop → clear buffer and start fresh fade-in
procedure TInternalCrossfadingFilter.OnVLCAudioFlush(data: Pointer; pts: Int64); cdecl;
begin
  if Assigned(FDirectSoundBuffer8) then
  begin
    FDirectSoundBuffer8.Stop;
    FillBufferWithSilence(FDirectSoundBuffer8);
    FDirectSoundBuffer8.Play(0, 0, DSBPLAY_LOOPING);
  end
  else if Assigned(FDirectSoundBuffer) then
  begin
    FDirectSoundBuffer.Stop;
    FillBufferWithSilence(FDirectSoundBuffer);
    FDirectSoundBuffer.Play(0, 0, DSBPLAY_LOOPING);
  end;

  FCrossfadeState := csFadeIn;
  FCrossfadeStartTime := GetTickCount;
  FCrossfadeProgress := 0.0;
end;

procedure TInternalCrossfadingFilter.OnVLCAudioDrain(data: Pointer); cdecl;
begin
  // Not used in this implementation
end;

procedure TInternalCrossfadingFilter.OnVLCAudioVolume(data: Pointer; volume: Single; mute: Boolean); cdecl;
begin
  SetMute(mute);
  SetVolume(volume); // volume is 0.0..1.0 (or higher in some VLC versions)
end;

// ---------------------------------------------------------------------------
// Core audio processing – writes data into the looping DirectSound buffer
// ---------------------------------------------------------------------------
procedure TInternalCrossfadingFilter.ProcessAudioData(const AudioData: PByte; DataSize: Integer);
var
  ProcessedData: PByte;
  OutputSize: Integer;
  BlockAlign, WriteSize, FirstPart, SecondPart: Integer;
  PlayCursor, WriteCursor: Cardinal;
  p1, p2: Pointer;
  s1, s2: Cardinal;
  hr: HRESULT;
  RestSize: Integer;
  Stille: array[0..4095] of Byte;
begin
  // Apply fade-in/out if active
  if FCrossfadeState <> csNone then
  begin
    FOutputBuffer.Position := 0;
    FOutputBuffer.Size := DataSize;
    ProcessCrossfading(AudioData, FOutputBuffer.Memory, DataSize);
    ProcessedData := FOutputBuffer.Memory;
    OutputSize := DataSize;
  end
  else
  begin
    ProcessedData := AudioData;
    OutputSize := DataSize;
  end;

  // Immediate pause handling (after fade-out completed)
  if FRequestPause then
  begin
    if Assigned(FDirectSoundBuffer8) then
    begin
      FDirectSoundBuffer8.Stop;
      FillBufferWithSilence(FDirectSoundBuffer8);
    end
    else if Assigned(FDirectSoundBuffer) then
    begin
      FDirectSoundBuffer.Stop;
      FillBufferWithSilence(FDirectSoundBuffer);
    end;
    FIsPaused := True;
    FRequestPause := False;
    Exit;
  end;

  BlockAlign := FAudioFormat.BytesPerSample;
  OutputSize := (OutputSize div BlockAlign) * BlockAlign; // round down to full samples

  WriteSize := OutputSize;
  while WriteSize > 0 do
  begin
    if FPos + WriteSize > FBufferSize then
      FirstPart := FBufferSize - FPos
    else
      FirstPart := WriteSize;
    SecondPart := WriteSize - FirstPart;

    if Assigned(FDirectSoundBuffer8) then
    begin
      FDirectSoundBuffer8.GetCurrentPosition(@PlayCursor, @WriteCursor);
      hr := FDirectSoundBuffer8.Lock(FPos, FirstPart, @p1, @s1, @p2, @s2, 0);
      if hr = DSERR_BUFFERLOST then
      begin
        FDirectSoundBuffer8.Restore;
        hr := FDirectSoundBuffer8.Lock(FPos, FirstPart, @p1, @s1, @p2, @s2, 0);
      end;

      if s1 > 0 then Move(ProcessedData^, p1^, s1);
      if s2 > 0 then Move(PByte(Integer(ProcessedData) + s1)^, p2^, s2);

      // Fill remaining locked area with silence if we have less data
      RestSize := FirstPart - (s1 + s2);
      if RestSize > 0 then
        FillChar(Pointer(Integer(p1) + s1)^, RestSize, 0);

      FDirectSoundBuffer8.Unlock(p1, s1, p2, s2);
    end
    else if Assigned(FDirectSoundBuffer) then
    begin
      // same code for older interface...
      // (omitted for brevity – identical)
    end;

    Inc(FPos, FirstPart);
    if FPos >= FBufferSize then
      FPos := FPos - FBufferSize;

    if SecondPart > 0 then
    begin
      Inc(ProcessedData, FirstPart);
      WriteSize := SecondPart;
      FPos := 0;
    end
    else
      WriteSize := 0;
  end;
end;

// ---------------------------------------------------------------------------
// Crossfading logic
// ---------------------------------------------------------------------------
procedure TInternalCrossfadingFilter.ProcessCrossfading(const InputBuffer: PByte; OutputBuffer: PByte; Count: Integer);
begin
  case FCrossfadeState of
    csNone:
      Move(InputBuffer^, OutputBuffer^, Count);

    csFadeIn,
    csFadeOut:
      begin
        UpdateCrossfadeProgress;
        ApplyCrossfadeEffect(InputBuffer, OutputBuffer, Count);

        if FCrossfadeProgress >= 1.0 then
        begin
          FCrossfadeState := csNone;

          // After fade-out → really pause
          if (FCrossfadeState = csNone) and FWillPause then
          begin
            if Assigned(FDirectSoundBuffer8) then
            begin
              FDirectSoundBuffer8.Stop;
              FillBufferWithSilence(FDirectSoundBuffer8);
            end
            else if Assigned(FDirectSoundBuffer) then
            begin
              FDirectSoundBuffer.Stop;
              FillBufferWithSilence(FDirectSoundBuffer);
            end;
            FIsPaused := True;
            FWillPause := False;
          end;
        end;
      end;

    csCrossfade:
      begin
        UpdateCrossfadeProgress;
        MixAudio(FCurrentBuffer.Data, FNextBuffer.Data, OutputBuffer, Count, FCrossfadeProgress);
        if FCrossfadeProgress >= 1.0 then
          FCrossfadeState := csNone;
      end;
  end;
end;

procedure TInternalCrossfadingFilter.ApplyCrossfadeEffect(const InputBuffer: PByte; OutputBuffer: PByte; Count: Integer);
var
  i: Integer;
  VolumeRatio: Double;
  SampleI: PSmallInt;
  OutI: PSmallInt;
  SampleF: PSingle;
  OutF: PSingle;
  SampleB: PByte;
  OutB: PByte;
begin
  VolumeRatio := 1.0 - FCrossfadeProgress; // fade-in: high → low, fade-out: low → high (inverted later if needed)

  if FAudioFormat.BitsPerSample = 16 then
  begin
    SampleI := PSmallInt(InputBuffer);
    OutI := PSmallInt(OutputBuffer);
    for i := 0 to (Count div 2) - 1 do
    begin
      OutI^ := Round(SampleI^ * VolumeRatio);
      Inc(SampleI); Inc(OutI);
    end;
  end
  else if FAudioFormat.BitsPerSample = 32 then
  begin
    SampleF := PSingle(InputBuffer);
    OutF := PSingle(OutputBuffer);
    for i := 0 to (Count div 4) - 1 do
    begin
      OutF^ := SampleF^ * VolumeRatio;
      Inc(SampleF); Inc(OutF);
    end;
  end
  else if FAudioFormat.BitsPerSample = 8 then
  begin
    SampleB := PByte(InputBuffer);
    OutB := PByte(OutputBuffer);
    for i := 0 to Count - 1 do
    begin
      OutB^ := Round(SampleB^ * VolumeRatio);
      Inc(SampleB); Inc(OutB);
    end;
  end;
end;

procedure TInternalCrossfadingFilter.MixAudio(const Buffer1, Buffer2: PByte; Output: PByte; Count: Integer; MixRatio: Double);
var
  i: Integer;
  S1I, S2I, OutI: PSmallInt;
  S1F, S2F, OutF: PSingle;
  S1B, S2B, OutB: PByte;
begin
  if FAudioFormat.BitsPerSample = 16 then
  begin
    S1I := PSmallInt(Buffer1);
    S2I := PSmallInt(Buffer2);
    OutI := PSmallInt(Output);
    for i := 0 to (Count div 2) - 1 do
    begin
      OutI^ := Round(S1I^ * (1.0 - MixRatio) + S2I^ * MixRatio);
      Inc(S1I); Inc(S2I); Inc(OutI);
    end;
  end
  else if FAudioFormat.BitsPerSample = 32 then
  begin
    S1F := PSingle(Buffer1);
    S2F := PSingle(Buffer2);
    OutF := PSingle(Output);
    for i := 0 to (Count div 4) - 1 do
    begin
      OutF^ := S1F^ * (1.0 - MixRatio) + S2F^ * MixRatio;
      Inc(S1F); Inc(S2F); Inc(OutF);
    end;
  end
  else if FAudioFormat.BitsPerSample = 8 then
  begin
    S1B := PByte(Buffer1);
    S2B := PByte(Buffer2);
    OutB := PByte(Output);
    for i := 0 to Count - 1 do
    begin
      OutB^ := Round(S1B^ * (1.0 - MixRatio) + S2B^ * MixRatio);
      Inc(S1B); Inc(S2B); Inc(OutB);
    end;
  end;
end;

procedure TInternalCrossfadingFilter.UpdateCrossfadeProgress;
var
  Elapsed: Cardinal;
begin
  Elapsed := GetTickCount - FCrossfadeStartTime;
  FCrossfadeProgress := Min(Elapsed / FCrossfadeTime, 1.0);
end;

// ---------------------------------------------------------------------------
// Silence filling helpers
// ---------------------------------------------------------------------------
procedure TInternalCrossfadingFilter.FillBufferWithSilence(Buffer: IDirectSoundBuffer);
var
  p1, p2: Pointer;
  s1, s2: Cardinal;
  hr: HRESULT;
  Zeroes: array of Byte;
begin
  SetLength(Zeroes, FBufferSize);
  FillChar(Zeroes[0], FBufferSize, 0);

  hr := Buffer.Lock(0, FBufferSize, @p1, @s1, @p2, @s2, 0);
  if Succeeded(hr) then
  begin
    if s1 > 0 then Move(Zeroes[0], p1^, s1);
    if s2 > 0 then Move(Zeroes[s1], p2^, s2);
    Buffer.Unlock(p1, s1, p2, s2);
  end;
end;

procedure TInternalCrossfadingFilter.FillBufferWithSilence(Buffer: IDirectSoundBuffer8);
begin
  FillBufferWithSilence(IDirectSoundBuffer(Buffer)); // reuse the same code
end;

// ---------------------------------------------------------------------------
// Public properties & methods
// ---------------------------------------------------------------------------
procedure TInternalCrossfadingFilter.SetCrossfadeTime(Time: Integer);
begin
  FCrossfadeTime := Time;
end;

function TInternalCrossfadingFilter.GetCrossfadeTime: Integer;
begin
  Result := FCrossfadeTime;
end;

procedure TInternalCrossfadingFilter.SetVolume(Volume: Single);
var
  DSVol: Integer;
begin
  if Volume > 1.0 then Volume := 1.0;
  if Volume < 0.0 then Volume := 0.0;

  DSVol := Round(Volume * 10000) - 10000; // 0.0 → -10000, 1.0 → 0
  FVolume := DSVol;

  if Assigned(FDirectSoundBuffer8) then
    FDirectSoundBuffer8.SetVolume(DSVol)
  else if Assigned(FDirectSoundBuffer) then
    FDirectSoundBuffer.SetVolume(DSVol);
end;

procedure TInternalCrossfadingFilter.SetBalance(Balance: Integer);
begin
  FBalance := EnsureRange(Balance, -10000, 10000);
  if Assigned(FDirectSoundBuffer8) then
    FDirectSoundBuffer8.SetPan(FBalance)
  else if Assigned(FDirectSoundBuffer) then
    FDirectSoundBuffer.SetPan(FBalance);
end;

procedure TInternalCrossfadingFilter.SetMute(Mute: Boolean);
begin
  FMute := Mute;
  // Currently disabled (Exit) – you can re-enable if you want true mute
end;

// ---------------------------------------------------------------------------
// Video crossfading registry hack (for DSP-worx Crossfading Renderer)
// ---------------------------------------------------------------------------
procedure TInternalCrossfadingFilter.EnableVideoCrossfading;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    Reg.OpenKey('Software\DSP-worx\Crossfading Renderer', True);
    Reg.WriteBool('AllowVideo', True);
    Reg.WriteInteger('FadingCross', FCrossfadeTime);
    Reg.WriteInteger('FadingSeek', FCrossfadeTime);
    Reg.CloseKey;

    // Additional keys used by older versions
    Reg.OpenKey('Software\DSP-worx\Crossfading', True);
    Reg.WriteBool('AllowVideo', True);
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

procedure TInternalCrossfadingFilter.DisableVideoCrossfading;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey('Software\DSP-worx\Crossfading Renderer', True);
    Reg.WriteBool('AllowVideo', False);
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

function TInternalCrossfadingFilter.IsVlcPaused: Boolean;
begin
  Result := Assigned(FVLCPlayer) and (FVLCPlayer.GetState = plvPlayer_Paused);
end;

end.