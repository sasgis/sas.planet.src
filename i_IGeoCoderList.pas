unit i_IGeoCoderList;

interface

uses
  ActiveX,
  i_JclNotify,
  i_GeoCoder;

type
  IGeoCoderListEntity = interface
    ['{FB6DA76B-1706-4F85-A2A0-53E61F4AED2F}']
    function GetGUID: TGUID;
    function GetCaption: WideString;
    function GetGeoCoder: IGeoCoder;
  end;

  IGeoCoderList = interface
    ['{34A0BB9F-8C6B-4664-B299-4F78710E0996}']
    function GetGUIDEnum: IEnumGUID;
    function Get(AGUID: TGUID): IGeoCoderListEntity;
    function GetAddNotifier: IJclNotifier;
  end;


implementation

end.
