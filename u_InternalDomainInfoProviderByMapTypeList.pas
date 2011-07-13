unit u_InternalDomainInfoProviderByMapTypeList;

interface

uses
  Classes,
  i_ConfigDataProvider,
  i_ZmpInfo,
  i_InternalDomainInfoProvider;

type
  TInternalDomainInfoProviderByMapTypeList = class(TInterfacedObject, IInternalDomainInfoProvider)
  private
    function ParseFilePath(AFilePath: string; out AZmpGUID: TGUID; out AFileName: string): Boolean;
    function LoadStreamFromZmp(AZmp: IZmpInfo; AFileName: string; AStream: TStream; out AContentType: string): Boolean;
    function LoadStreamFromDataProvider(ADataProvider: IConfigDataProvider; AFileName: string; AStream: TStream; out AContentType: string): Boolean;
    function LoadStreamFromSubDataProvider(ADataProvider: IConfigDataProvider; AFileName: string; AStream: TStream; out AContentType: string): Boolean;
  protected
    function LoadStreamByFilePath(AFilePath: string; AStream: TStream; out AContentType: string): Boolean;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  i_ContentTypeManager,
  i_ContentTypeInfo,
  u_MapType,
  c_ZeroGUID,
  u_GlobalState;

const
  CFileNameSeparator = '/';

{ TInternalDomainInfoProviderByMapTypeList }

function TInternalDomainInfoProviderByMapTypeList.LoadStreamByFilePath(
  AFilePath: string; AStream: TStream; out AContentType: string): Boolean;
var
  VGuid: TGUID;
  VMapType: TMapType;
  VFileName: string;
begin
  Result := ParseFilePath(AFilePath, VGuid, VFileName);
  if Result then begin
    Result := False;
    VMapType := GState.MapType.GetMapFromID(VGuid);
    if VMapType <> nil then begin
      Result := LoadStreamFromZmp(VMapType.Zmp, VFileName, AStream, AContentType);
    end;
  end;
end;

function TInternalDomainInfoProviderByMapTypeList.LoadStreamFromDataProvider(
  ADataProvider: IConfigDataProvider; AFileName: string; AStream: TStream;
  out AContentType: string): Boolean;
var
  VFileName: string;
  VExt: string;
  VContentTypeManager: IContentTypeManager;
  VContentType: IContentTypeInfoBasic;
begin
  AContentType := '';
  VFileName := AFileName;
  if VFileName = '' then begin
    VFileName := 'index.html';
  end;
  if AContentType = '' then begin
    VExt := ExtractFileExt(VFileName);
    VContentTypeManager := GState.ContentTypeManager;
    VContentType := VContentTypeManager.GetInfoByExt(VExt);
    if VContentType <> nil then begin
      AContentType := VContentType.GetContentType;
    end else begin
      AContentType := 'text/html'
    end;
  end;

  Result := ADataProvider.ReadBinaryStream(VFileName, AStream) > 0;
end;

function TInternalDomainInfoProviderByMapTypeList.LoadStreamFromSubDataProvider(
  ADataProvider: IConfigDataProvider; AFileName: string; AStream: TStream;
  out AContentType: string): Boolean;
var
  VSubItemName: string;
  VFileName: string;
  VPos: Integer;
  VSubItemProvider: IConfigDataProvider;
begin
  VSubItemName := '';
  VFileName := '';
  VPos := Pos(CFileNameSeparator, AFileName);
  if VPos > 0 then begin
    VSubItemName := LeftStr(AFileName, VPos - 1);
    VFileName := RightStr(AFileName, Length(AFileName) - VPos - Length(CFileNameSeparator) + 1);
    if VSubItemName <> '' then begin
      VSubItemProvider := ADataProvider.GetSubItem(VSubItemName);
    end else begin
      VSubItemProvider := ADataProvider;
    end;
    if VSubItemProvider <> nil then begin
      Result := LoadStreamFromSubDataProvider(VSubItemProvider, VFileName, AStream, AContentType);
    end else begin
      Result := False;
    end;
  end else begin
    VFileName := AFileName;
    Result := LoadStreamFromDataProvider(ADataProvider, VFileName, AStream, AContentType);
  end;
end;

function TInternalDomainInfoProviderByMapTypeList.LoadStreamFromZmp(
  AZmp: IZmpInfo; AFileName: string; AStream: TStream; out AContentType: string): Boolean;
begin
  Result := LoadStreamFromSubDataProvider(AZmp.DataProvider, AFileName, AStream, AContentType);
end;

function TInternalDomainInfoProviderByMapTypeList.ParseFilePath(
  AFilePath: string; out AZmpGUID: TGUID; out AFileName: string): Boolean;
var
  VGUIDString: string;
  VPos: Integer;
begin
  AZmpGUID := CGUID_Zero;
  AFileName := '';

  VPos := Pos(CFileNameSeparator, AFilePath);
  if VPos > 0 then begin
    VGUIDString := LeftStr(AFilePath, VPos - 1);
    AFileName := RightStr(AFilePath, Length(AFilePath) - VPos - Length(CFileNameSeparator) + 1);
  end else begin
    VGUIDString := AFilePath;
  end;
  if Length(VGUIDString) > 0 then begin
    try
      AZmpGUID := StringToGUID(VGUIDString);
    except
      AZmpGUID := CGUID_Zero;
    end;
  end;
  Result := not IsEqualGUID(AZmpGUID, CGUID_Zero)
end;

end.
