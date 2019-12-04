unit SASPlanet.modules;

// This unit must be first unit in dpr file

interface

{$I SASPlanet.inc}

uses
  {$IFDEF USE_FAST_MM}
  FastMM4,
  {$ENDIF}
  {$IFDEF USE_FAST_MOVE}
  FastMove,
  {$ENDIF}
  {$IFDEF USE_FAST_CODE}
  FastCode,
  {$ENDIF}
  {$IF CompilerVersion < 21.0} // CompilerVersion < Delphi 2010
  MidasSpeedFix,
  {$IFEND}
  {$IF (CompilerVersion < 23) or (CompilerVersion >= 27)}
  MidasLib, // bug in MidasLib.dcu in XE2..XE5 http://qc.embarcadero.com/wc/qcmain.aspx?d=109476
  {$IFEND}
  WinInetFix,
  XPMan;

implementation

{$IF DEFINED(RELEASE) and DEFINED(USE_FAST_MM)}
  {$IF CompilerVersion >= 32}
    {$I FastMM4Options.inc}
    {$IFDEF DetectMMOperationsAfterUninstall}
      // bug in Delphi 10.2 and up cause FastMM error:
      // "FastMM has detected a FreeMem call after FastMM was uninstalled"
      // Delphi bug-report: https://quality.embarcadero.com/browse/RSP-22897
      // SASPlanet issue: http://www.sasgis.org/mantis/view.php?id=3466
      {$MESSAGE ERROR 'DetectMMOperationsAfterUninstall (FastMM4Options.inc) must be undefined!'}
    {$ENDIF}
  {$IFEND}
{$IFEND}

end.
