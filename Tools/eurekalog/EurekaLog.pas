unit EurekaLog;

interface

uses
  ExceptionLog,
  ECore,
  ETypes,
  Windows,
  ShellAPI;

implementation

// Make extended configuration EurekaLog here //

procedure EurekaHelpButtonClickNotify(EurekaExceptionRecord: TEurekaExceptionRecord; var CloseDialog: Boolean);
begin
  // За хелпом отправляем на форум, в тему "SASPlanet - отладочные версии"
  ShellExecute(0, 'open', 'http://sasgis.org/forum/viewtopic.php?f=47&t=1508', nil, nil, SW_SHOW)
end;

initialization
  CustomButtonClickNotify := EurekaHelpButtonClickNotify;

end.
