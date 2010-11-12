unit u_ContentTypeManagerSimple;

interface

uses
  Classes,
  i_ContentTypeInfo,
  i_IContentConverter,
  i_IContentTypeManager,
  u_ContentTypeListByKey;

type
  TContentTypeManagerSimple = class(TInterfacedObject, IContentTypeManager)
  private
    FExtList: TContentTypeListByKey;
    FTypeList: TContentTypeListByKey;
    FBitmapExtList: TContentTypeListByKey;
    FBitmapTypeList: TContentTypeListByKey;
    FKmlExtList: TContentTypeListByKey;
    FKmlTypeList: TContentTypeListByKey;
    FConvertersBySourceTypeList: TStringList;
  protected
    procedure AddByType(AInfo: IContentTypeInfoBasic; AType: string);
    procedure AddByExt(AInfo: IContentTypeInfoBasic; AExt: string);
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
  SysUtils;

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
  FConvertersBySourceTypeList := TStringList.Create;
end;

destructor TContentTypeManagerSimple.Destroy;
begin
  FreeAndNil(FExtList);
  FreeAndNil(FTypeList);
  FreeAndNil(FBitmapExtList);
  FreeAndNil(FBitmapTypeList);
  FreeAndNil(FKmlExtList);
  FreeAndNil(FKmlTypeList);
  FreeAndNil(FConvertersBySourceTypeList);
  inherited;
end;

function TContentTypeManagerSimple.GetConverter(ATypeSource,
  ATypeTarget: WideString): IContentConverter;
begin

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

end.
