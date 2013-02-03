unit i_ErrorInfo;

interface

uses
  Types,
  i_LonLatRect,
  i_CoordConverter;

type
  IErrorInfoSimple = interface
    ['{53FB6DC2-73AB-4A32-8CA2-BDC129298810}']
    function GetErrorText: string;
    property ErrorText: string read GetErrorText;
  end;

  IErrorInfoWithCaption = interface
    ['{744830D6-D864-40CA-AAA0-F608D0F9CD4B}']
    function GetCaption: string;
    property Caption: string read GetCaption;
  end;

  IErrorInfoWithGeoRect = interface
    ['{4DBF2561-0D5E-42FA-8A0A-DFB78FD548F5}']
    function GetRect: ILonLatRect;
    property Rect: ILonLatRect read GetRect;
  end;

  IErrorInfoTile = interface
    ['{B10B733B-BA3F-416D-B9B1-9BDDE576BB30}']
    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetTile: TPoint;
    property Tile: TPoint read GetTile;
  end;

  IErrorInfoMapType = interface
    ['{2A88F07B-6C27-45E9-B37C-A1F58EE6008B}']
    function GetMapTypeGUID: TGUID;
    property MapTypeGUID: TGUID read GetMapTypeGUID;
  end;

implementation

end.
