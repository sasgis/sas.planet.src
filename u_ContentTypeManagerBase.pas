unit u_ContentTypeManagerBase;

interface

uses
  i_ContentTypeInfo,
  i_ContentConverter,
  i_ContentTypeManager,
  u_ContentTypeListByKey,
  u_ContentConverterMatrix;

type
  TContentTypeManagerBase = class(TInterfacedObject, IContentTypeManager)
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
    property ExtList: TContentTypeListByKey read FExtList;
    property TypeList: TContentTypeListByKey read FTypeList;
    property BitmapExtList: TContentTypeListByKey read FBitmapExtList;
    property BitmapTypeList: TContentTypeListByKey read FBitmapTypeList;
    property KmlExtList: TContentTypeListByKey read FKmlExtList;
    property KmlTypeList: TContentTypeListByKey read FKmlTypeList;
    property ConverterMatrix: TContentConverterMatrix read FConverterMatrix;

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

procedure TContentTypeManagerBase.AddByExt(AInfo: IContentTypeInfoBasic;
  AExt: string);
begin
  FExtList.Add(AExt, AInfo);
  if Supports(AInfo, IContentTypeInfoBitmap) then begin
    FBitmapExtList.Add(AExt, AInfo);
  end else if Supports(AInfo, IContentTypeInfoKml) then begin
    FKmlExtList.Add(AExt, AInfo);
  end;
end;

procedure TContentTypeManagerBase.AddByType(AInfo: IContentTypeInfoBasic;
  AType: string);
begin
  FTypeList.Add(AType, AInfo);
  if Supports(AInfo, IContentTypeInfoBitmap) then begin
    FBitmapTypeList.Add(AType, AInfo);
  end else if Supports(AInfo, IContentTypeInfoKml) then begin
    FKmlTypeList.Add(AType, AInfo);
  end;
end;

constructor TContentTypeManagerBase.Create;
begin
  FExtList := TContentTypeListByKey.Create;
  FTypeList := TContentTypeListByKey.Create;
  FBitmapExtList := TContentTypeListByKey.Create;
  FBitmapTypeList := TContentTypeListByKey.Create;
  FKmlExtList := TContentTypeListByKey.Create;
  FKmlTypeList := TContentTypeListByKey.Create;
  FConverterMatrix := TContentConverterMatrix.Create;
end;

destructor TContentTypeManagerBase.Destroy;
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

function TContentTypeManagerBase.GetConverter(ATypeSource,
  ATypeTarget: WideString): IContentConverter;
begin
  Result := FConverterMatrix.Get(ATypeSource, ATypeTarget);
end;

function TContentTypeManagerBase.GetInfo(
  AType: WideString): IContentTypeInfoBasic;
begin
  Result := FTypeList.Get(AType);
end;

function TContentTypeManagerBase.GetInfoByExt(
  AExt: WideString): IContentTypeInfoBasic;
begin
  Result := FExtList.Get(AExt);
end;

function TContentTypeManagerBase.GetIsBitmapExt(AExt: WideString): Boolean;
begin
  Result := FBitmapExtList.Get(AExt) <> nil;
end;

function TContentTypeManagerBase.GetIsBitmapType(AType: WideString): Boolean;
begin
  Result := FBitmapTypeList.Get(AType) <> nil;
end;

function TContentTypeManagerBase.GetIsKmlExt(AExt: WideString): Boolean;
begin
  Result := FKmlExtList.Get(AExt) <> nil;
end;

function TContentTypeManagerBase.GetIsKmlType(AType: WideString): Boolean;
begin
  Result := FKmlTypeList.Get(AType) <> nil;
end;

end.
