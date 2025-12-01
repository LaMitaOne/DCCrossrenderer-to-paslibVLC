unit LaMitaVLCVisuals;

interface

uses
  Windows, Classes, SysUtils, Math, DirectSound, PasLibVlcUnit, PasLibVlcPlayerUnit,
  {$IFDEF FPC}LCLType,{$ENDIF} Messages, MMSystem, DXTypes, Registry,
  visWaveform, visSpectrum;

type
  // Audio-Format fÃ¼r Crossfading
  TAudioFormat = record
    SampleRate: Cardinal;
    Channels: Cardinal;
    BitsPerSample: Cardinal;
    BytesPerSample: Cardinal;
  end;

  // Crossfading-Zustand
  TCrossfadeState = (csNone, csFadeIn, csFadeOut, csCrossfade);

  // Audio-Buffer fÃ¼r Crossfading
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

  // Interner Crossfading-Filter
  TLaMitaVLCVisuals = class
  private
    // DirectSound
    FDirectSound: IDirectSound;
    FDirectSoundBuffer: IDirectSoundBuffer;
    FDirectSoundBuffer8: IDirectSoundBuffer8;
    
    // Audio-Format
    FAudioFormat: TAudioFormat;
    
    // Crossfading
    FCrossfadeState: TCrossfadeState;
    FCrossfadeTime: Integer; // in ms
    FCrossfadeProgress: Double; // 0.0 - 1.0
    FCrossfadeStartTime: Cardinal;
    
    // Audio-Buffer
    FCurrentBuffer: TAudioBuffer;
    FNextBuffer: TAudioBuffer;
    FOutputBuffer: TMemoryStream; // Changed from TAudioBuffer to TMemoryStream
    
    // Volume/Balance
    FVolume: Integer; // -10000 to 0
    FBalance: Integer; // -10000 to 10000
    FMute: Boolean;
    
    // Format-Erkennung
    FFormatDetected: Boolean;
    
    // Buffer-Position (wie DSPack)
    FPos: DWORD;
    FBufferSize: DWORD;
    
    
  
  // VLC-Player Referenz
  FVLCPlayer: TPasLibVlcPlayer;
    
    FIsPaused: Boolean;
    FRequestPause: Boolean;
    FHasFadedOut: Boolean;
    FWillPause: Boolean;
    FReadyForAudio: Boolean;
    FBufferReady: Boolean;

    // Visualizer
    FWaveform: TDCWaveform;
    FSpectrum: TDCSpectrum;
    
    // DirectSound-Methoden
    function CreateDirectSoundBuffer: Boolean;
    function InitializeDirectSound: Boolean;
    procedure CleanupDirectSound;
    
    // Crossfading-Methoden
    procedure ProcessCrossfading(const InputBuffer: PByte; OutputBuffer: PByte; Count: Integer);
    procedure ApplyCrossfadeEffect(const InputBuffer: PByte; OutputBuffer: PByte; Count: Integer);
    procedure UpdateCrossfadeProgress;
    
    // Audio-Verarbeitung
    procedure ProcessAudioData(const AudioData: PByte; DataSize: Integer);
    procedure MixAudio(const Buffer1, Buffer2: PByte; Output: PByte; Count: Integer; MixRatio: Double);
    
    procedure FillBufferWithSilence(Buffer: IDirectSoundBuffer); overload;
    procedure FillBufferWithSilence(Buffer: IDirectSoundBuffer8); overload;
  public
    constructor Create;
    destructor Destroy; override;
    
    // VLC-Integration
    function AttachToVLCPlayer(VLCPlayer: TPasLibVlcPlayer): Boolean;
    procedure DetachFromVLCPlayer;
    
    // Crossfading-Einstellungen
    procedure SetCrossfadeTime(Time: Integer); // in ms
    function GetCrossfadeTime: Integer;
    
    // Volume/Balance
    procedure SetVolume(Volume: Single); // -10000 to 0
    function GetVolume: Integer;
    procedure SetBalance(Balance: Integer); // -10000 to 10000
    function GetBalance: Integer;
    procedure SetMute(Mute: Boolean);
    function GetMute: Boolean;
    
    // Filter-Status
    function IsActive: Boolean;
    procedure StartCrossfade;
    procedure StopCrossfade;
    
    // Video-Crossfading
    procedure EnableVideoCrossfading;
    procedure DisableVideoCrossfading;
    
    // Audio-Format
    procedure SetAudioFormat(SampleRate, Channels, BitsPerSample: Cardinal);
    function GetAudioFormat: TAudioFormat;
    
    // Callbacks fÃ¼r VLC (als normale Methoden)
    procedure OnVLCAudioPlay(data: Pointer; samples: Pointer; count: Cardinal; pts: Int64); cdecl;
    procedure OnVLCAudioPause(data: Pointer; pts: Int64); cdecl;
    procedure OnVLCAudioResume(data: Pointer; pts: Int64); cdecl;
    procedure OnVLCAudioFlush(data: Pointer; pts: Int64); cdecl;
    procedure OnVLCAudioDrain(data: Pointer); cdecl;
    procedure OnVLCAudioVolume(data: Pointer; volume: Single; mute: Boolean); cdecl;
    function IsVlcPaused: Boolean;
    property Waveform: TDCWaveform read FWaveform write FWaveform;
    property Spectrum: TDCSpectrum read FSpectrum write FSpectrum;
  end;

implementation

type
  TFadeOutThread = class(TThread)
  private
    FFilter: TLaMitaVLCVisuals;
  protected
    procedure Execute; override;
  public
    constructor Create(Filter: TLaMitaVLCVisuals);
  end;

var
  GlobalFilter: TLaMitaVLCVisuals = nil;
  FadeOutThread: TFadeOutThread = nil;

constructor TFadeOutThread.Create(Filter: TLaMitaVLCVisuals);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FFilter := Filter;
end;

procedure TFadeOutThread.Execute;
var
  Dummy: array[0..4095] of Byte;
begin
  while not Terminated and Assigned(FFilter) and (FFilter.FCrossfadeState = csFadeOut) do
  begin
    FillChar(Dummy, SizeOf(Dummy), 0);
    FFilter.ProcessAudioData(@Dummy, SizeOf(Dummy));
    // Buffer nach jedem Write explizit auf Play setzen
    if Assigned(FFilter.FDirectSoundBuffer8) then
      FFilter.FDirectSoundBuffer8.Play(0, 0, DSBPLAY_LOOPING)
    else if Assigned(FFilter.FDirectSoundBuffer) then
      FFilter.FDirectSoundBuffer.Play(0, 0, DSBPLAY_LOOPING);
    Sleep(10);
  end;
end;

// Globale Variable fÃ¼r den Filter-Zeiger

// Wrapper-Funktionen fÃ¼r VLC-Callbacks
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

// Format-Callback fÃ¼r VLC (ohne User-Data)
procedure AudioFormatCallback(data: Pointer; format: PAnsiChar; rate, channels: Cardinal); cdecl;
var
  FormatChanged: Boolean;
  NewBitsPerSample: Cardinal;
begin
  try
    // Debug-Ausgabe: Format-String IMMER ausgeben
 //   OutputDebugString('AudioFormatCallback: format=' + string(format));
    if (GlobalFilter = nil) or (not GlobalFilter.FReadyForAudio) then
    begin
      OutputDebugString('AudioFormatCallback: Noch nicht bereit!');
      Exit;
    end;
  //  OutputDebugString('AudioFormatCallback: Eingehende Werte: format=' + string(format) + ' rate=' + IntToStr(rate) + ' channels=' + IntToStr(channels));
    // Harte Werte-Checks und Fallbacks
    if (rate < 8000) or (rate > 192000) then
    begin
      OutputDebugString('AudioFormatCallback: UngÃ¼ltige SampleRate! Fallback auf 44100');
      rate := 44100;
    end;
    if (channels < 1) or (channels > 8) then
    begin
      OutputDebugString('AudioFormatCallback: UngÃ¼ltige Channels! Fallback auf 2');
      channels := 2;
    end;
    // BitsPerSample aus Format-String bestimmen
    if SameText(format, 'S16N') or SameText(format, 's16n') then
      NewBitsPerSample := 16
    else if SameText(format, 'U8') or SameText(format, 'u8') then
      NewBitsPerSample := 8
    else
      NewBitsPerSample := 16; // Fallback
    with GlobalFilter do
    begin
      FormatChanged := (FAudioFormat.SampleRate <> rate) or
                       (FAudioFormat.Channels <> channels) or
                       (FAudioFormat.BitsPerSample <> NewBitsPerSample);
      if FormatChanged then
      begin
        OutputDebugString('AudioFormatCallback: Formatwechsel erkannt!');
        if Assigned(FDirectSoundBuffer8) then FDirectSoundBuffer8.Stop;
        if Assigned(FDirectSoundBuffer) then FDirectSoundBuffer.Stop;
        FAudioFormat.SampleRate := rate;
        FAudioFormat.Channels := channels;
        FAudioFormat.BitsPerSample := NewBitsPerSample;
        FAudioFormat.BytesPerSample := (FAudioFormat.Channels * FAudioFormat.BitsPerSample) div 8;
        try
          if not CreateDirectSoundBuffer then
          begin
            FBufferReady := False;
            OutputDebugString('AudioFormatCallback: Buffer-Create fehlgeschlagen!');
            Exit;
          end;
          FBufferReady := True;
          if Assigned(FDirectSoundBuffer8) then FDirectSoundBuffer8.Play(0, 0, DSBPLAY_LOOPING);
          if Assigned(FDirectSoundBuffer) then FDirectSoundBuffer.Play(0, 0, DSBPLAY_LOOPING);
        except
          on E: Exception do
          begin
            OutputDebugString(PChar('CreateDirectSoundBuffer Exception: ' + E.Message));
            FBufferReady := False;
          end;
        end;
        OutputDebugString(PChar('AudioFormatCallback: Neues Format: ' + string(format) + ' SR=' + IntToStr(rate) + ' Ch=' + IntToStr(channels) + ' Bits=' + IntToStr(NewBitsPerSample)));
      end;
    end;
  except
    on E: Exception do
      OutputDebugString(PChar('AudioFormatCallback Exception: ' + E.Message));
  end;
end;

{ TAudioBuffer }

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
  inherited Destroy;
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

{ TLaMitaVLCVisuals }

constructor TLaMitaVLCVisuals.Create;
begin
  inherited Create;
  
  // DirectSound initialisieren
  if not InitializeDirectSound then
    raise Exception.Create('Failed to initialize DirectSound');
  
  // Audio-Format standardmÃ¤ÃŸig setzen (wird von VLC Ã¼berschrieben)
  FAudioFormat.SampleRate := 44100; // Standard, wird von VLC Ã¼berschrieben
  FAudioFormat.Channels := 2;
  FAudioFormat.BitsPerSample := 16;
  FAudioFormat.BytesPerSample := 4; // 2 channels * 16 bit / 8
  
  // Crossfading-Einstellungen
  FCrossfadeTime := 2000; // 2 Sekunden
  FCrossfadeProgress := 0.0;
  FCrossfadeState := csNone;
  
  // Format-Erkennung
  FFormatDetected := False;
  
  // Buffer-Position (wie DSPack)
  FPos := 0;
  FBufferSize := 0;
  
  // Volume/Balance - NORMAL setzen (nicht Maximum)
  FVolume := -1000; // -10dB statt Maximum
  FBalance := 0; // Center
  FMute := False;
  
  // Audio-Buffer erstellen (48kHz)
  FCurrentBuffer := TAudioBuffer.Create(48000 * 4 * 2); // 2 Sekunden Buffer
  FNextBuffer := TAudioBuffer.Create(48000 * 4 * 2);
  FOutputBuffer := TMemoryStream.Create; // Changed from TAudioBuffer to TMemoryStream
  
  FVLCPlayer := nil;
  FIsPaused := False;
  FRequestPause := False;
  FHasFadedOut := False;
  FWillPause := False;
  FReadyForAudio := False;
  FBufferReady := False;

  FWaveform := TDCWaveform.Create(nil);
  FSpectrum := TDCSpectrum.Create(nil);
  
  // Buffer mit Standardwerten anlegen
  if not CreateDirectSoundBuffer then
    raise Exception.Create('Failed to create DirectSound buffer');
  FBufferReady := True;

end;

destructor TLaMitaVLCVisuals.Destroy;
begin
  DetachFromVLCPlayer;
  
  FCurrentBuffer.Free;
  FNextBuffer.Free;
  if Assigned(FOutputBuffer) then FOutputBuffer.Free;
  FWaveform.Free;
  FSpectrum.Free;
  
  CleanupDirectSound;
  
  inherited Destroy;
end;

function TLaMitaVLCVisuals.InitializeDirectSound: Boolean;
var
  hr: HRESULT;
begin
  Result := False;
  
  // Debug: DirectSound-Initialisierung
  OutputDebugString('Initialisiere DirectSound...');
  
  // Standard-Device verwenden (SoundBlaster wird automatisch erkannt)
  hr := DirectSoundCreate(nil, FDirectSound, nil);
  
  if Failed(hr) then 
  begin
    OutputDebugString(PChar('DirectSoundCreate fehlgeschlagen: ' + IntToStr(hr)));
    Exit;
  end;
  
  OutputDebugString('DirectSoundCreate erfolgreich');
  
  // Cooperative Level setzen
  hr := FDirectSound.SetCooperativeLevel(GetDesktopWindow, DSSCL_PRIORITY);
  if Failed(hr) then 
  begin
    OutputDebugString(PChar('SetCooperativeLevel fehlgeschlagen: ' + IntToStr(hr)));
    Exit;
  end;
  
  OutputDebugString('DirectSound-Initialisierung erfolgreich');
  Result := True;
end;

function TLaMitaVLCVisuals.CreateDirectSoundBuffer: Boolean;
var
  hr: HRESULT;
  WaveFormat: TWaveFormatEx;
  BufferDesc: TDSBufferDesc;
begin
  Result := False;
  OutputDebugString('Erstelle DirectSound Buffer...');
  FillChar(WaveFormat, SizeOf(WaveFormat), 0);
  WaveFormat.wFormatTag := WAVE_FORMAT_PCM;
  WaveFormat.nChannels := FAudioFormat.Channels;
  WaveFormat.nSamplesPerSec := FAudioFormat.SampleRate;
  WaveFormat.wBitsPerSample := FAudioFormat.BitsPerSample;
  WaveFormat.nBlockAlign := (WaveFormat.nChannels * WaveFormat.wBitsPerSample) div 8;
  WaveFormat.nAvgBytesPerSec := WaveFormat.nSamplesPerSec * WaveFormat.nBlockAlign;
  OutputDebugString(PChar('Audio Format: ' + IntToStr(WaveFormat.nSamplesPerSec) + 'Hz, ' + IntToStr(WaveFormat.nChannels) + ' Channels, ' + IntToStr(WaveFormat.wBitsPerSample) + ' Bits'));
  FillChar(BufferDesc, SizeOf(BufferDesc), 0);
  BufferDesc.dwSize := SizeOf(BufferDesc);
  BufferDesc.dwFlags := DSBCAPS_GLOBALFOCUS or DSBCAPS_GETCURRENTPOSITION2 or DSBCAPS_STICKYFOCUS;
  // AX-Logik: BuffergrÃ¶ÃŸe berechnen (z.B. 2 Sekunden Buffer)
  FBufferSize := FAudioFormat.SampleRate * FAudioFormat.BytesPerSample * 2;
  BufferDesc.dwBufferBytes := FBufferSize;
  BufferDesc.lpwfxFormat := @WaveFormat;
  hr := FDirectSound.CreateSoundBuffer(BufferDesc, FDirectSoundBuffer, nil);
  if Failed(hr) then 
  begin
    OutputDebugString(PChar('CreateSoundBuffer fehlgeschlagen: ' + IntToStr(hr)));
    Exit;
  end;
  OutputDebugString('CreateSoundBuffer erfolgreich');
  try
    hr := FDirectSoundBuffer.QueryInterface(IID_IDirectSoundBuffer8, FDirectSoundBuffer8);
    if Failed(hr) then
    begin
      OutputDebugString('IDirectSoundBuffer8 nicht verfÃ¼gbar, verwende IDirectSoundBuffer');
      FDirectSoundBuffer8 := nil;
    end
    else
      OutputDebugString('IDirectSoundBuffer8 erfolgreich');
  except
    OutputDebugString('Exception beim QueryInterface fÃ¼r IDirectSoundBuffer8');
    FDirectSoundBuffer8 := nil;
  end;
  OutputDebugString('DirectSound Buffer erfolgreich erstellt');
  // AX-Logik: FPos nach jedem neuen Buffer-Create auf 0 setzen
  FPos := 0;
  Result := True;
end;

procedure TLaMitaVLCVisuals.CleanupDirectSound;
begin
  FDirectSoundBuffer8 := nil;
  FDirectSoundBuffer := nil;
  FDirectSound := nil;
end;

function TLaMitaVLCVisuals.AttachToVLCPlayer(VLCPlayer: TPasLibVlcPlayer): Boolean;
begin
  Result := False;
  
  if not Assigned(VLCPlayer) then 
  begin
    Exit;
  end;
  
  FVLCPlayer := VLCPlayer;
  
  // Globalen Filter setzen
  GlobalFilter := Self;
  
  // VLC Audio-Callbacks setzen
  libvlc_audio_set_callbacks(
    FVLCPlayer.GetPlayerHandle,
    @VLCPlayCallback,
    @VLCPauseCallback,
    @VLCResumeCallback,
    @VLCFlushCallback,
    @VLCDrainCallback,
    Self
  );
  
  // Audio-Format explizit setzen (VLC muss es verwenden)
  libvlc_audio_set_format(
    FVLCPlayer.GetPlayerHandle,
    'S16N', // 16-bit Stereo, Native Endian
    44100,  // 44.1kHz (Standard fÃ¼r Audio)
    2       // Stereo
  );

  // Format-Callback registrieren
  FReadyForAudio := True;
  libvlc_audio_set_format_callbacks(
    FVLCPlayer.GetPlayerHandle,
    @AudioFormatCallback,
    nil
  );
  
  // Volume-Callback setzen
  libvlc_audio_set_volume_callback(
    FVLCPlayer.GetPlayerHandle,
    @VLCVolumeCallback
  );
  
  // Video-Crossfading aktivieren (wie im AX Crossfader)
  EnableVideoCrossfading;
  
  FCrossfadeState := csFadeIn;
  FCrossfadeStartTime := GetTickCount;
  FCrossfadeProgress := 0.0;

  // Buffer mit Standardwerten anlegen
  if not CreateDirectSoundBuffer then
    Exit;
  FBufferReady := True;

  if Assigned(FDirectSoundBuffer8) then
  begin
    FillBufferWithSilence(FDirectSoundBuffer8);
    FDirectSoundBuffer8.Play(0, 0, DSBPLAY_LOOPING);
    FDirectSoundBuffer8.SetVolume(FVolume);
  end
  else if Assigned(FDirectSoundBuffer) then
  begin
    FillBufferWithSilence(FDirectSoundBuffer);
    FDirectSoundBuffer.Play(0, 0, DSBPLAY_LOOPING);
    FDirectSoundBuffer.SetVolume(FVolume);
  end;
  
  Result := True;
end;

procedure TLaMitaVLCVisuals.DetachFromVLCPlayer;
begin
  if Assigned(FVLCPlayer) then
  begin
    // Globalen Filter zurÃ¼cksetzen
    if GlobalFilter = Self then
      GlobalFilter := nil;
    
    // Callbacks entfernen
    libvlc_audio_set_callbacks(
      FVLCPlayer.GetPlayerHandle,
      nil, nil, nil, nil, nil, nil
    );

    libvlc_audio_set_format_callbacks(
      FVLCPlayer.GetPlayerHandle,
      nil, nil
    );
    
    libvlc_audio_set_volume_callback(
      FVLCPlayer.GetPlayerHandle,
      nil
    );
    
    FVLCPlayer := nil;
    FReadyForAudio := False;
    FBufferReady := False;
  end;
end;

procedure TLaMitaVLCVisuals.OnVLCAudioPlay(data: Pointer; samples: Pointer; count: Cardinal; pts: Int64); cdecl;
begin
  // Debug-Ausgabe: Format und Callback-Infos
  OutputDebugString(PChar('OnVLCAudioPlay: SR=' + IntToStr(FAudioFormat.SampleRate) + ' Ch=' + IntToStr(FAudioFormat.Channels) + ' Bits=' + IntToStr(FAudioFormat.BitsPerSample) + ' Count=' + IntToStr(count) + ' pts=' + IntToStr(pts)));

  // Audio-Daten verarbeiten (wie DSPack DoRenderSample)
  // IMMER das Format aus FAudioFormat verwenden (wird im Format-Callback gesetzt)
  if FIsPaused or (not FBufferReady) then
    Exit; // Keine neuen Daten schreiben, wenn wir im Pausenmodus sind oder Buffer nicht bereit ist
  ProcessAudioData(PByte(samples), count * FAudioFormat.BytesPerSample);
  // PCM-Daten an Visualizer weitergeben
  if Assigned(FWaveform) then
    FWaveform.Process(samples, count * FAudioFormat.BytesPerSample, FAudioFormat.BitsPerSample, FAudioFormat.Channels, False);
  if Assigned(FSpectrum) then
    FSpectrum.Process(samples, count * FAudioFormat.BytesPerSample, FAudioFormat.BitsPerSample, FAudioFormat.Channels, False);
end;

procedure TLaMitaVLCVisuals.OnVLCAudioPause(data: Pointer; pts: Int64); cdecl;
begin
  FIsPaused := False;
  FHasFadedOut := False;
  FWillPause := True; // Merken, dass nach FadeOut wirklich Pause ist
  FCrossfadeState := csFadeOut;
  FCrossfadeStartTime := GetTickCount;
  FCrossfadeProgress := 0.0;
  // Eigenen FadeOut-Thread starten
  if FadeOutThread = nil then
    FadeOutThread := TFadeOutThread.Create(Self);
end;

procedure TLaMitaVLCVisuals.OnVLCAudioResume(data: Pointer; pts: Int64); cdecl;
begin
  FIsPaused := False;
  FRequestPause := False;
  FHasFadedOut := False;
  FWillPause := False;
  if Assigned(FVLCPlayer) then
    FVLCPlayer.SetAudioMute(False); // LautstÃ¤rke wieder normal
  FCrossfadeState := csFadeIn;
  FCrossfadeStartTime := GetTickCount;
  FCrossfadeProgress := 0.0;
  if Assigned(FVLCPlayer) then
    libvlc_audio_set_callbacks(
      FVLCPlayer.GetPlayerHandle,
      @VLCPlayCallback,
      @VLCPauseCallback,
      @VLCResumeCallback,
      @VLCFlushCallback,
      @VLCDrainCallback,
      Self
    );
  if Assigned(FDirectSoundBuffer8) then
    FDirectSoundBuffer8.Play(0, 0, DSBPLAY_LOOPING)
  else if Assigned(FDirectSoundBuffer) then
    FDirectSoundBuffer.Play(0, 0, DSBPLAY_LOOPING);
end;

procedure TLaMitaVLCVisuals.OnVLCAudioFlush(data: Pointer; pts: Int64); cdecl;
begin
  OutputDebugString('VLC Audio Flush - Buffer leeren und FadeIn starten');
  
  // Audio-Buffer leeren (wie im AX-Filter)
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
  
  // Crossfade-Status zurÃ¼cksetzen (wie im AX-Filter)
  FCrossfadeState := csFadeIn;
  FCrossfadeStartTime := GetTickCount;
  FCrossfadeProgress := 0.0;

  OutputDebugString('Buffer geleert, FadeIn gestartet');
end;

procedure TLaMitaVLCVisuals.OnVLCAudioDrain(data: Pointer); cdecl;
begin
  // Audio-Buffer drainen
  // Warten bis alle Daten abgespielt sind
end;

procedure TLaMitaVLCVisuals.OnVLCAudioVolume(data: Pointer; volume: Single; mute: Boolean); cdecl;
begin
  // Debug: Volume-Callback
  OutputDebugString(PChar('VLC Volume Callback: ' + FloatToStr(volume) + ', Mute: ' + BoolToStr(mute)));
  // Volume setzen
  SetMute(mute);
  SetVolume(volume);
  //if volume > 1.0 then
  //  volume := 1.0;
  //  SetVolume((1.0 - Min(volume, 1.0)) * -10000);
  //  SetVolume(Round((1.0 - Min(volume, 1.0)) * 10000) * -1);
  //  SetVolume(Round(volume * -10000));
 // SetVolume(-10000 + Round(volume * 10000));
end;

procedure TLaMitaVLCVisuals.ProcessAudioData(const AudioData: PByte; DataSize: Integer);
var
  PlayCursor, WriteCursor: Cardinal;
  p1, p2: Pointer;
  s1, s2: Cardinal;
  hr: HRESULT;
  ProcessedData: PByte;
  OutputSize: Integer;
  BlockAlign: Integer;
  Stille: array[0..4095] of Byte;
  RestSize: Integer;
  WriteSize, FirstPart, SecondPart: Integer;
begin
  // FOutputBuffer nur neu anlegen, wenn nÃ¶tig!
  if (FOutputBuffer = nil) then
    FOutputBuffer := TMemoryStream.Create;
  if FOutputBuffer.Size <> DataSize then
    FOutputBuffer.Size := DataSize;

  // Crossfade/FadeIn/FadeOut anwenden, wenn nÃ¶tig
  if FCrossfadeState <> csNone then
  begin
    FOutputBuffer.Position := 0;
    ProcessCrossfading(AudioData, FOutputBuffer.Memory, DataSize);
    ProcessedData := FOutputBuffer.Memory;
    OutputSize := DataSize;
  end
  else
  begin
    ProcessedData := AudioData;
    OutputSize := DataSize;
  end;

  if FIsPaused then
    Exit;

  // Entfernt: Buffer-Stopp bei FRequestPause (AX-Logik: nur nach FadeOut-Ende stoppen)
  // if FRequestPause then
  // begin
  //   if Assigned(FDirectSoundBuffer8) then
  //   begin
  //     FDirectSoundBuffer8.Stop;
  //     FillBufferWithSilence(FDirectSoundBuffer8);
  //     FDirectSoundBuffer8.SetCurrentPosition(0); // Falls verfügbar!
  //   end
  //   else if Assigned(FDirectSoundBuffer) then
  //   begin
  //     FDirectSoundBuffer.Stop;
  //     FillBufferWithSilence(FDirectSoundBuffer);
  //     FDirectSoundBuffer.SetCurrentPosition(0); // Falls verfügbar!
  //   end;
  //   FIsPaused := True;
  //   FRequestPause := False;
  //   if Assigned(FVLCPlayer) then
  //     libvlc_audio_set_callbacks(FVLCPlayer.GetPlayerHandle, nil, nil, nil, nil, nil, nil);
  //   Exit;
  // end;

  // AX-Logik: BlockAlign berechnen
  BlockAlign := (FAudioFormat.Channels * FAudioFormat.BitsPerSample) div 8;
  // DataSize auf Vielfaches von BlockAlign runden
  OutputSize := (OutputSize div BlockAlign) * BlockAlign;

  // AX-Logik: Ringbuffer-Handling
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
        FDirectSoundBuffer8.Lock(FPos, FirstPart, @p1, @s1, @p2, @s2, 0);
      end;
      if (s1 > 0) then Move(ProcessedData^, p1^, s1);
      if (s2 > 0) then Move(PChar(ProcessedData)[s1], p2^, s2);
      // Stille auffÃ¼llen, wenn weniger Daten geliefert wurden
      RestSize := FirstPart - (s1 + s2);
      if RestSize > 0 then
      begin
        FillChar(Stille, SizeOf(Stille), 0);
        if RestSize <= SizeOf(Stille) then
          Move(Stille, Pointer(Integer(p1) + s1)^, RestSize)
        else
          FillChar(Pointer(Integer(p1) + s1)^, RestSize, 0);
      end;
      FDirectSoundBuffer8.Unlock(p1, s1, p2, s2);
    end
    else if Assigned(FDirectSoundBuffer) then
    begin
      FDirectSoundBuffer.GetCurrentPosition(@PlayCursor, @WriteCursor);
      hr := FDirectSoundBuffer.Lock(FPos, FirstPart, @p1, @s1, @p2, @s2, 0);
      if hr = DSERR_BUFFERLOST then
      begin
        FDirectSoundBuffer.Restore;
        FDirectSoundBuffer.Lock(FPos, FirstPart, @p1, @s1, @p2, @s2, 0);
      end;
      if (s1 > 0) then Move(ProcessedData^, p1^, s1);
      if (s2 > 0) then Move(PChar(ProcessedData)[s1], p2^, s2);
      // Stille auffÃ¼llen, wenn weniger Daten geliefert wurden
      RestSize := FirstPart - (s1 + s2);
      if RestSize > 0 then
      begin
        FillChar(Stille, SizeOf(Stille), 0);
        if RestSize <= SizeOf(Stille) then
          Move(Stille, Pointer(Integer(p1) + s1)^, RestSize)
        else
          FillChar(Pointer(Integer(p1) + s1)^, RestSize, 0);
      end;
      FDirectSoundBuffer.Unlock(p1, s1, p2, s2);
    end;
    // AX-Logik: FPos aktualisieren
    Inc(FPos, FirstPart);
    if FPos >= FBufferSize then
      Dec(FPos, FBufferSize);
    // NÃ¤chster Block (falls Ringbuffer-Umbruch)
    if SecondPart > 0 then
    begin
      ProcessedData := PByte(Integer(ProcessedData) + FirstPart);
      WriteSize := SecondPart;
      FPos := 0;
    end
    else
      WriteSize := 0;
  end;
  // Debug-Ausgabe fÃ¼r Buffer-Logik
  OutputDebugString(PChar('ProcessAudioData: OutputSize=' + IntToStr(OutputSize) + ' FBufferSize=' + IntToStr(FBufferSize) + ' FPos=' + IntToStr(FPos)));
end;

// ProcessCrossfading mit Eingabe-/Ausgabepuffer
procedure TLaMitaVLCVisuals.ProcessCrossfading(const InputBuffer: PByte; OutputBuffer: PByte; Count: Integer);
begin
  case FCrossfadeState of
    csNone:
      begin
        Move(InputBuffer^, OutputBuffer^, Count);
      end;
    csFadeIn:
      begin
        UpdateCrossfadeProgress;
        ApplyCrossfadeEffect(InputBuffer, OutputBuffer, Count);
        if FCrossfadeProgress >= 1.0 then
          FCrossfadeState := csNone;
      end;
    csFadeOut:
      begin
        UpdateCrossfadeProgress;
        ApplyCrossfadeEffect(InputBuffer, OutputBuffer, Count);
        if FCrossfadeProgress >= 1.0 then
        begin
          FCrossfadeState := csNone;
          // AX-Logik: Pause erst nach FadeOut!
          if FWillPause then
          begin
            if Assigned(FDirectSoundBuffer8) then
            begin
              FDirectSoundBuffer8.Stop;
              FillBufferWithSilence(FDirectSoundBuffer8);
              FDirectSoundBuffer8.SetCurrentPosition(0);
            end
            else if Assigned(FDirectSoundBuffer) then
            begin
              FDirectSoundBuffer.Stop;
              FillBufferWithSilence(FDirectSoundBuffer);
              FDirectSoundBuffer.SetCurrentPosition(0);
            end;
            if Assigned(FVLCPlayer) then
              FVLCPlayer.SetAudioMute(True);
            FIsPaused := True;
            FWillPause := False;
            // Thread nach FadeOut beenden
            if Assigned(FadeOutThread) then
            begin
              FadeOutThread.Terminate;
              FadeOutThread := nil;
            end;
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

// In ApplyCrossfadeEffect und MixAudio:
// Passe die Verarbeitung an das aktuelle Format an (hier exemplarisch fÃ¼r 16 Bit, 32 Bit, 8 Bit)
procedure TLaMitaVLCVisuals.ApplyCrossfadeEffect(const InputBuffer: PByte; OutputBuffer: PByte; Count: Integer);
var
  i: Integer;
  VolumeRatio: Double;
  Sample: PSmallInt;
  OutSample: PSmallInt;
  SampleF: PSingle;
  OutSampleF: PSingle;
  SampleB: PByte;
  OutSampleB: PByte;
begin
  VolumeRatio := 1.0 - FCrossfadeProgress;
  if FAudioFormat.BitsPerSample = 16 then
  begin
    Sample := PSmallInt(InputBuffer);
    OutSample := PSmallInt(OutputBuffer);
    for i := 0 to (Count div 2) - 1 do
    begin
      OutSample^ := Round(Sample^ * VolumeRatio);
      Inc(Sample);
      Inc(OutSample);
    end;
  end
  else if FAudioFormat.BitsPerSample = 32 then
  begin
    SampleF := PSingle(InputBuffer);
    OutSampleF := PSingle(OutputBuffer);
    for i := 0 to (Count div 4) - 1 do
    begin
      OutSampleF^ := SampleF^ * VolumeRatio;
      Inc(SampleF);
      Inc(OutSampleF);
    end;
  end
  else if FAudioFormat.BitsPerSample = 8 then
  begin
    SampleB := PByte(InputBuffer);
    OutSampleB := PByte(OutputBuffer);
    for i := 0 to Count - 1 do
    begin
      OutSampleB^ := Round(SampleB^ * VolumeRatio);
      Inc(SampleB);
      Inc(OutSampleB);
    end;
  end;
end;

procedure TLaMitaVLCVisuals.MixAudio(const Buffer1, Buffer2: PByte; Output: PByte; Count: Integer; MixRatio: Double);
var
  i: Integer;
  Sample1: PSmallInt;
  Sample2: PSmallInt;
  OutSample: PSmallInt;
  Sample1F: PSingle;
  Sample2F: PSingle;
  OutSampleF: PSingle;
  Sample1B: PByte;
  Sample2B: PByte;
  OutSampleB: PByte;
begin
  if FAudioFormat.BitsPerSample = 16 then
  begin
    Sample1 := PSmallInt(Buffer1);
    Sample2 := PSmallInt(Buffer2);
    OutSample := PSmallInt(Output);
    for i := 0 to (Count div 2) - 1 do
    begin
      OutSample^ := Round(Sample1^ * (1.0 - MixRatio) + Sample2^ * MixRatio);
      Inc(Sample1);
      Inc(Sample2);
      Inc(OutSample);
    end;
  end
  else if FAudioFormat.BitsPerSample = 32 then
  begin
    Sample1F := PSingle(Buffer1);
    Sample2F := PSingle(Buffer2);
    OutSampleF := PSingle(Output);
    for i := 0 to (Count div 4) - 1 do
    begin
      OutSampleF^ := Sample1F^ * (1.0 - MixRatio) + Sample2F^ * MixRatio;
      Inc(Sample1F);
      Inc(Sample2F);
      Inc(OutSampleF);
    end;
  end
  else if FAudioFormat.BitsPerSample = 8 then
  begin
    Sample1B := PByte(Buffer1);
    Sample2B := PByte(Buffer2);
    OutSampleB := PByte(Output);
    for i := 0 to Count - 1 do
    begin
      OutSampleB^ := Round(Sample1B^ * (1.0 - MixRatio) + Sample2B^ * MixRatio);
      Inc(Sample1B);
      Inc(Sample2B);
      Inc(OutSampleB);
    end;
  end;
end;

procedure TLaMitaVLCVisuals.FillBufferWithSilence(Buffer: IDirectSoundBuffer);
var
  p1, p2: Pointer;
  s1, s2: Cardinal;
  hr: HRESULT;
  ZeroBuffer: array of Byte;
begin
  if FBufferSize = 0 then Exit;
  SetLength(ZeroBuffer, FBufferSize);
  FillChar(ZeroBuffer[0], FBufferSize, 0);

  hr := Buffer.Lock(0, FBufferSize, @p1, @s1, @p2, @s2, 0);
  if hr = DS_OK then
  begin
    if s1 > 0 then
      Move(ZeroBuffer[0], p1^, s1);
    if s2 > 0 then
      Move(ZeroBuffer[s1], p2^, s2);
    Buffer.Unlock(p1, s1, p2, s2);
  end;
end;

procedure TLaMitaVLCVisuals.FillBufferWithSilence(Buffer: IDirectSoundBuffer8);
var
  p1, p2: Pointer;
  s1, s2: Cardinal;
  hr: HRESULT;
  ZeroBuffer: array of Byte;
begin
  // Buffer mit 0 fÃ¼llen (wie DSPack)
  SetLength(ZeroBuffer, FBufferSize);
  FillChar(ZeroBuffer[0], FBufferSize, 0);
  
  hr := Buffer.Lock(0, FBufferSize, @p1, @s1, @p2, @s2, 0);
  if hr = DS_OK then
  begin
    if s1 > 0 then
      Move(ZeroBuffer[0], p1^, s1);
    if s2 > 0 then
      Move(ZeroBuffer[s1], p2^, s2);
    Buffer.Unlock(p1, s1, p2, s2);
  end;
end;

procedure TLaMitaVLCVisuals.UpdateCrossfadeProgress;
var
  CurrentTime: Cardinal;
  ElapsedTime: Cardinal;
begin
  CurrentTime := GetTickCount;
  ElapsedTime := CurrentTime - FCrossfadeStartTime;
  
  FCrossfadeProgress := Min(ElapsedTime / FCrossfadeTime, 1.0);
  
  if FCrossfadeProgress >= 1.0 then
  begin
    // Crossfading abgeschlossen
    FCrossfadeState := csNone;
    FCrossfadeProgress := 0.0;
  end;
end;

procedure TLaMitaVLCVisuals.SetCrossfadeTime(Time: Integer);
begin
  FCrossfadeTime := Time;
end;

function TLaMitaVLCVisuals.GetCrossfadeTime: Integer;
begin
  Result := FCrossfadeTime;
end;

procedure TLaMitaVLCVisuals.SetVolume(Volume: Single);
var
  DSVolume: Integer;
begin
  // Begrenzen auf 0..1
  if Volume > 1.0 then
    Volume := 1.0;
  if Volume < 0.0 then
    Volume := 0.0;

  // Umrechnung auf DirectSound (-10000 = stumm, 0 = max)
  DSVolume := Round(Volume * 10000 - 10000);

  FVolume := DSVolume;

  OutputDebugString(PChar('SetVolume aufgerufen: ' + IntToStr(FVolume)));
  
  if Assigned(FDirectSoundBuffer8) then
  begin
    FDirectSoundBuffer8.SetVolume(FVolume);
    OutputDebugString('Volume an DirectSoundBuffer8 gesetzt');
  end
  else if Assigned(FDirectSoundBuffer) then
  begin
    FDirectSoundBuffer.SetVolume(FVolume);
    OutputDebugString('Volume an DirectSoundBuffer gesetzt');
  end
  else
    OutputDebugString('Kein DirectSound Buffer verfÃ¼gbar!');
end;

function TLaMitaVLCVisuals.GetVolume: Integer;
begin
  Result := FVolume;
end;

procedure TLaMitaVLCVisuals.SetBalance(Balance: Integer);
begin
  FBalance := EnsureRange(Balance, -10000, 10000);
  
  if Assigned(FDirectSoundBuffer8) then
    FDirectSoundBuffer8.SetPan(FBalance)
  else if Assigned(FDirectSoundBuffer) then
    FDirectSoundBuffer.SetPan(FBalance);
end;

function TLaMitaVLCVisuals.GetBalance: Integer;
begin
  Result := FBalance;
end;

procedure TLaMitaVLCVisuals.SetMute(Mute: Boolean);
begin
  Exit;
  FMute := Mute;

  if Assigned(FDirectSoundBuffer8) then
  begin
    if FMute then
      FDirectSoundBuffer8.SetVolume(-10000)
    else
      FDirectSoundBuffer8.SetVolume(FVolume);
  end
  else if Assigned(FDirectSoundBuffer) then
  begin
    if FMute then
      FDirectSoundBuffer.SetVolume(-10000)
    else
      FDirectSoundBuffer.SetVolume(FVolume);
  end;
end;

function TLaMitaVLCVisuals.GetMute: Boolean;
begin
  Result := FMute;
end;

function TLaMitaVLCVisuals.IsActive: Boolean;
begin
  Result := Assigned(FVLCPlayer) and Assigned(FDirectSoundBuffer);
end;

procedure TLaMitaVLCVisuals.StartCrossfade;
begin
  FCrossfadeStartTime := GetTickCount;
  FCrossfadeProgress := 0.0;
  FCrossfadeState := csCrossfade;
end;

procedure TLaMitaVLCVisuals.StopCrossfade;
begin
  FCrossfadeState := csNone;
  FCrossfadeProgress := 0.0;
end;

procedure TLaMitaVLCVisuals.SetAudioFormat(SampleRate, Channels, BitsPerSample: Cardinal);
begin
  FAudioFormat.SampleRate := SampleRate;
  FAudioFormat.Channels := Channels;
  FAudioFormat.BitsPerSample := BitsPerSample;
  FAudioFormat.BytesPerSample := (Channels * BitsPerSample) div 8;
end;

function TLaMitaVLCVisuals.GetAudioFormat: TAudioFormat;
begin
  Result := FAudioFormat;
end;

procedure TLaMitaVLCVisuals.EnableVideoCrossfading;
var
  MyReg: TRegistry;
begin
  // Video-Crossfading in Registry aktivieren (wie im AX Crossfader)
  MyReg := TRegistry.Create;
  try
    MyReg.RootKey := HKEY_CURRENT_USER;
    MyReg.OpenKey('Software\DSP-worx\Crossfading Renderer', true);
    MyReg.WriteBool('AllowVideo', True);
    MyReg.WriteInteger('FadingCross', FCrossfadeTime);
    MyReg.WriteInteger('FadingSeek', FCrossfadeTime);
    MyReg.CloseKey;
    OutputDebugString('Video-Crossfading in Registry aktiviert');
    
    // Auch fÃ¼r andere mÃ¶gliche Registry-Pfade
    MyReg.OpenKey('Software\DSP-worx\Crossfading', true);
    MyReg.WriteBool('AllowVideo', True);
    MyReg.WriteInteger('FadingCross', FCrossfadeTime);
    MyReg.WriteInteger('FadingSeek', FCrossfadeTime);
    MyReg.CloseKey;
    OutputDebugString('Video-Crossfading in alternativer Registry aktiviert');
    
    // Auch fÃ¼r Video-Crossfading Units
    MyReg.OpenKey('Software\DSP-worx\Video Crossfading', true);
    MyReg.WriteBool('Enabled', True);
    MyReg.WriteInteger('FadeTime', FCrossfadeTime);
    MyReg.CloseKey;
    OutputDebugString('Video-Crossfading Units aktiviert');
    
  finally
    MyReg.Free;
  end;
end;

procedure TLaMitaVLCVisuals.DisableVideoCrossfading;
var
  MyReg: TRegistry;
begin
  // Video-Crossfading in Registry deaktivieren
  MyReg := TRegistry.Create;
  try
    MyReg.RootKey := HKEY_CURRENT_USER;
    MyReg.OpenKey('Software\DSP-worx\Crossfading Renderer', true);
    MyReg.WriteBool('AllowVideo', False);
    MyReg.CloseKey;
    OutputDebugString('Video-Crossfading in Registry deaktiviert');
  finally
    MyReg.Free;
  end;
end;

// Hilfsfunktion fÃ¼r Statusvergleich
function TLaMitaVLCVisuals.IsVlcPaused: Boolean;
begin
  Result := Assigned(FVLCPlayer) and (FVLCPlayer.GetState = plvPlayer_Paused);
end;


end.