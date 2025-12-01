# DCCrossrenderer-to-paslibVLC
  
  
A try to get dccrossrenderer connected to paslibvlc...not working fully (maybe some of you can get it running)

# InternalCrossfadingFilter

Just a quick experiment I played around with...

**Goal:**  
Hook the DCCrossrenderer / DSP-worx crossfading logic directly to PasLibVLC without any ActiveX/COM   
    
**What works:**  
- The connection from PasLibVLC → custom DirectSound buffer works  
- Audio comes through, format changes are detected, buffer is recreated

**What doesn’t work (at all):**  
- Fade-out before pause → never happens, instant stop with pop  
- Fade-out only starts after next play  
- Pause/resume/seek problems  
    
Probably we must render it all self there or something i thought of...  
  
i came to that idea when i tried to connect dcdsp visuals to paslibvlc, i put that unit here too  
    
Someone who has more plan of DCCrossrenderer and PasLibVLC maybe could get it done in few minutes :P  
i dont know, but anyway thought i post it here so maybe someone will find it :)  
    
  
