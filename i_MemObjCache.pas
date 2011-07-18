unit i_MemObjCache;

interface

uses
  GR32,
  i_VectorDataItemSimple;

type
  IMemObjCacheBitmap = interface
    ['{1CF92025-6BA9-4264-91E8-73766E698A6D}']
    procedure Clear;
    procedure DeleteFileFromCache(AKey: string);
    procedure AddTileToCache(AObj: TCustomBitmap32; AKey: string);
    function TryLoadFileFromCache(AObj: TCustomBitmap32; AKey: string): boolean;
  end;

  IMemObjCacheVector = interface
    ['{0BB1598E-A00C-4BBE-9AA8-08F94974EAB2}']
    procedure Clear;
    procedure DeleteFileFromCache(AKey: string);
    procedure AddTileToCache(AObj: IVectorDataItemList; AKey: string);
    function TryLoadFileFromCache(var AObj: IVectorDataItemList; AKey: string): boolean;
  end;

implementation

end.
 