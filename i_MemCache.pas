unit i_MemCache;

interface

uses
  Types;
type
  ICacheElement = interface
  ['{3E56D8A9-51CB-4CB0-AABF-CD09145CEFB1}']
    function GetIsEmpty: Boolean;
    function GetPutTime: TDateTime;
    function GetLastAccessTime: TDateTime;
    procedure GetTileCoord(var AXY: TPoint; var AZoom: Byte);
    function GetObject: TObject;
  end;

  IMemCache = interface
  ['{75B5851E-3BC3-41B4-9E2E-8804AB073BCB}']
    function GetByCoord(AXY: TPoint; AZoom: Byte): ICacheElement;
    procedure PutObject(AXY: TPoint; AZoom: Byte; AObj: TObject);
    procedure TrimByTimeToLive;
    procedure TrimByCount;
  end;
implementation

end.
