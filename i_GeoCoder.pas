unit i_GeoCoder;

interface

uses
  ActiveX,
  t_GeoTypes;

type
  IGeoCodeResult = interface
    ['{C90929AD-3A6C-4906-A554-E1DA363ED060}']
    function GetSearchText: WideString; safecall;
    function GetResultCode: Integer; safecall;
    function GetMessage: WideString; safecall;
    function GetPlacemarks: IEnumUnknown; safecall;
    function GetPlacemarksCount: integer; safecall;
  end;

  IGeoCodePalcemark = interface
    ['{744CAB70-0466-433A-AF57-00BD5AFD9F45}']
    function GetPoint: TDoublePoint; safecall;
    function GetAddress: WideString; safecall;
    function GetAccuracy: Integer; safecall;
  end;

  IGeoCoder = interface
    ['{D9293293-080A-44B7-92F8-3093D35A551B}']
    function GetLocations(ASearch: WideString; ACurrentPos: TDoublePoint): IGeoCodeResult; safecall;
  end;

implementation

end.
 