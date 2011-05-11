unit i_MemObjCache;

interface

uses
  GR32,
  i_VectorDataItemSimple;

type
  IMemObjCache = interface
    ['{1CF92025-6BA9-4264-91E8-73766E698A6D}']
    procedure Clear;
    procedure DeleteFileFromCache(AKey: string);
    procedure AddTileToCache(AObj: TCustomBitmap32; AKey: string); overload;
    procedure AddTileToCache(AObj: IVectorDataItemList; AKey: string); overload;
    function TryLoadFileFromCache(AObj: TCustomBitmap32; AKey: string): boolean; overload;
    function TryLoadFileFromCache(var AObj: IVectorDataItemList; AKey: string): boolean; overload;
  end;

implementation

end.
 