unit SASPlanet.modles;

interface

// This unit must be first unit in dpr file.
uses
  {$IFDEF RELEASE}
    FastMM4,
    FastMove,
    {$IFNDEF UNICODE}
      FastCode,
    {$ENDIF}
  {$ENDIF}
  {$IF CompilerVersion < 21.0} // CompilerVersion < Delphi 2010
    MidasSpeedFix,
  {$IFEND}
  {$IF (CompilerVersion < 23) or (CompilerVersion >= 27)}
    MidasLib, // bug in MidasLib.dcu in XE2..XE5 http://qc.embarcadero.com/wc/qcmain.aspx?d=109476
  {$IFEND}
  XPMan;

implementation

end.
