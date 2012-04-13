unit i_InvisibleBrowser;

interface

type
  IInvisibleBrowser = interface
    ['{18EFF2B1-208D-421E-A71F-633A47BA9C96}']
    procedure NavigateAndWait(const AUrl: WideString);
  end;

implementation

end.
