unit i_IMemObjCache;

interface

uses
  GR32,
  u_KmlInfoSimple;

type
  IMemObjCache = interface
  ['{1CF92025-6BA9-4264-91E8-73766E698A6D}']
    procedure Clear;
    procedure DeleteFileFromCache(AKey: string);
    procedure AddTileToCache(AObj: TBitmap32; AKey: string); overload;
    procedure AddTileToCache(AObj: TKmlInfoSimple; AKey: string); overload;
    function TryLoadFileFromCache(AObj: TBitmap32; AKey: string):boolean; overload;
    function TryLoadFileFromCache(AObj: TKmlInfoSimple; AKey: string):boolean; overload;
  end;

implementation

end.
 