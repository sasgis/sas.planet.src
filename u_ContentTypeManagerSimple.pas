unit u_ContentTypeManagerSimple;

interface

uses
  i_ContentTypeInfo,
  i_IContentConverter,
  i_IContentTypeManager,
  u_ContentTypeListByKey,
  u_ContentConverterMatrix;

type
  TContentTypeManagerSimple = class(TInterfacedObject, IContentTypeManager)
  private
    FExtList: TContentTypeListByKey;
    FTypeList: TContentTypeListByKey;
    FBitmapExtList: TContentTypeListByKey;
    FBitmapTypeList: TContentTypeListByKey;
    FKmlExtList: TContentTypeListByKey;
    FKmlTypeList: TContentTypeListByKey;
    FConverterMatrix: TContentConverterMatrix;
  protected
    procedure AddByType(AInfo: IContentTypeInfoBasic; AType: string);
    procedure AddByExt(AInfo: IContentTypeInfoBasic; AExt: string);
    procedure UpdateBitmapConverterMatrix;
    procedure InitLists;
  protected
    function GetInfo(AType: WideString): IContentTypeInfoBasic;
    function GetInfoByExt(AExt: WideString): IContentTypeInfoBasic;
    function GetIsBitmapType(AType: WideString): Boolean;
    function GetIsBitmapExt(AExt: WideString): Boolean;
    function GetIsKmlType(AType: WideString): Boolean;
    function GetIsKmlExt(AExt: WideString): Boolean;
    function GetConverter(ATypeSource, ATypeTarget: WideString): IContentConverter;
  public
    constructor Create();
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  SysUtils,
  u_ContentTypeInfo,
  u_ContentConverterKmz2Kml,
  u_ContentConverterKml2Kmz,
  u_BitmapTileJpegLoader,
  u_BitmapTileJpegSaverIJL,
  u_BitmapTileVampyreLoader,
  u_BitmapTileVampyreSaver,
  u_ContentConverterBitmap;

{ TContentTypeManagerSimple }

procedure TContentTypeManagerSimple.AddByExt(AInfo: IContentTypeInfoBasic;
  AExt: string);
begin
  FExtList.Add(AExt, AInfo);
  if Supports(AInfo, IContentTypeInfoBitmap) then begin
    FBitmapExtList.Add(AExt, AInfo);
  end else if Supports(AInfo, IContentTypeInfoBitmap) then begin
    FKmlExtList.Add(AExt, AInfo);
  end;
end;

procedure TContentTypeManagerSimple.AddByType(AInfo: IContentTypeInfoBasic;
  AType: string);
begin
  FTypeList.Add(AType, AInfo);
  if Supports(AInfo, IContentTypeInfoBitmap) then begin
    FBitmapTypeList.Add(AType, AInfo);
  end else if Supports(AInfo, IContentTypeInfoBitmap) then begin
    FKmlTypeList.Add(AType, AInfo);
  end;
end;

constructor TContentTypeManagerSimple.Create;
begin
  FExtList := TContentTypeListByKey.Create;
  FTypeList := TContentTypeListByKey.Create;
  FBitmapExtList := TContentTypeListByKey.Create;
  FBitmapTypeList := TContentTypeListByKey.Create;
  FKmlExtList := TContentTypeListByKey.Create;
  FKmlTypeList := TContentTypeListByKey.Create;
  FConverterMatrix := TContentConverterMatrix.Create;
  InitLists;
end;

destructor TContentTypeManagerSimple.Destroy;
begin
  FreeAndNil(FExtList);
  FreeAndNil(FTypeList);
  FreeAndNil(FBitmapExtList);
  FreeAndNil(FBitmapTypeList);
  FreeAndNil(FKmlExtList);
  FreeAndNil(FKmlTypeList);
  FreeAndNil(FConverterMatrix);
  inherited;
end;

function TContentTypeManagerSimple.GetConverter(ATypeSource,
  ATypeTarget: WideString): IContentConverter;
begin
  Result := FConverterMatrix.Get(ATypeSource, ATypeTarget);
end;

function TContentTypeManagerSimple.GetInfo(
  AType: WideString): IContentTypeInfoBasic;
begin
  Result := FTypeList.Get(AType);
end;

function TContentTypeManagerSimple.GetInfoByExt(
  AExt: WideString): IContentTypeInfoBasic;
begin
  Result := FExtList.Get(AExt);
end;

function TContentTypeManagerSimple.GetIsBitmapExt(AExt: WideString): Boolean;
begin
  Result := FBitmapExtList.Get(AExt) <> nil;
end;

function TContentTypeManagerSimple.GetIsBitmapType(AType: WideString): Boolean;
begin
  Result := FBitmapTypeList.Get(AType) <> nil;
end;

function TContentTypeManagerSimple.GetIsKmlExt(AExt: WideString): Boolean;
begin
  Result := FKmlExtList.Get(AExt) <> nil;
end;

function TContentTypeManagerSimple.GetIsKmlType(AType: WideString): Boolean;
begin
  Result := FKmlTypeList.Get(AType) <> nil;
end;

procedure TContentTypeManagerSimple.InitLists;
var
  VContentType: IContentTypeInfoBasic;
begin
  VContentType := TContentTypeInfoBitmap.Create(
    'image/jpg',
    '.jpg',
    TJpegBitmapTileLoader.Create,
    TJpegBitmapTileSaverIJL.Create(85)
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByType(VContentType, 'image/jpeg');
  AddByType(VContentType, 'image/pjpeg');
  AddByExt(VContentType, VContentType.GetDefaultExt);
  AddByExt(VContentType, '.jpeg');
  
  VContentType := TContentTypeInfoBitmap.Create(
    'image/png',
    '.png',
    TVampyreBasicBitmapTileLoaderPNG.Create,
    TVampyreBasicBitmapTileSaverPNG.Create
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByType(VContentType, 'image/x-png');
  AddByType(VContentType, 'image/png; mode=24bit');
  AddByExt(VContentType, VContentType.GetDefaultExt);

  VContentType := TContentTypeInfoBitmap.Create(
    'image/gif',
    '.gif',
    TVampyreBasicBitmapTileLoaderGIF.Create,
    TVampyreBasicBitmapTileSaverGIF.Create
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByExt(VContentType, VContentType.GetDefaultExt);

  VContentType := TContentTypeInfoBitmap.Create(
    'image/bmp',
    '.bmp',
    TVampyreBasicBitmapTileLoaderBMP.Create,
    TVampyreBasicBitmapTileSaverBMP.Create
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByType(VContentType, 'image/x-ms-bmp');
  AddByType(VContentType, 'image/x-windows-bmp');
  AddByExt(VContentType, VContentType.GetDefaultExt);

end;

procedure TContentTypeManagerSimple.UpdateBitmapConverterMatrix;
var
  VSourceEnumerator: TStringsEnumerator;
  VSoruceName: string;
  VSourceContent: IContentTypeInfoBitmap;
  VTargetEnumerator: TStringsEnumerator;
  VTargetName: string;
  VTargetContent: IContentTypeInfoBitmap;
  VConverter: IContentConverter;
begin
  VSourceEnumerator := FBitmapTypeList.GetEnumerator;
  try
    VTargetEnumerator := FBitmapTypeList.GetEnumerator;
    try
      while VSourceEnumerator.MoveNext do begin
        VSoruceName := VSourceEnumerator.Current;
        VSourceContent := FBitmapTypeList.Get(VSoruceName) as IContentTypeInfoBitmap;
        if VSourceContent.GetLoader <> nil then begin
          while VTargetEnumerator.MoveNext do begin
            VTargetName := VTargetEnumerator.Current;
            VTargetContent := FBitmapTypeList.Get(VTargetName) as IContentTypeInfoBitmap;
            if VTargetContent.GetSaver <> nil then begin
              VConverter := TContentConverterBitmap.Create(VSourceContent, VTargetContent);
              FConverterMatrix.Add(VSoruceName, VTargetName, VConverter);
            end;
          end;
        end;
      end;
    finally
      VSourceEnumerator.Free;
    end;
  finally
    VSourceEnumerator.Free;
  end;
end;

end.
