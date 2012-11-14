unit i_InternalBrowser;

interface



type
  IInternalBrowser = interface
    ['{FDCA8E35-D4BB-43A5-8A06-AC6C13C78B6F}']
    procedure ShowMessage(const ACaption, AText: string);
    procedure Navigate(const ACaption, AUrl: string);
    procedure NavigatePost(const ACaption, AUrl, AReferer, APostData: string);
  end;

implementation

end.
