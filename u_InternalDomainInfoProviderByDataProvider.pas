unit u_InternalDomainInfoProviderByDataProvider;

interface

uses
  i_BinaryData,
  i_ConfigDataProvider,
  i_ContentTypeManager,
  i_InternalDomainInfoProvider,
  u_BaseInterfacedObject;

type
  TInternalDomainInfoProviderByDataProvider = class(TBaseInterfacedObject, IInternalDomainInfoProvider)
  private
    FContentTypeManager: IContentTypeManager;
    FProvider: IConfigDataProvider;

    function LoadDataFromSubDataProvider(
      const ADataProvider: IConfigDataProvider;
      const AFileName: string;
      out AContentType: string
    ): IBinaryData;
    function LoadDataFromDataProvider(
      const ADataProvider: IConfigDataProvider;
      const AFileName: string;
      out AContentType: string
    ): IBinaryData;
  private
    function LoadBinaryByFilePath(
      const AFilePath: string;
      out AContentType: string
    ): IBinaryData;
  public
    constructor Create(
      const AProvider: IConfigDataProvider;
      const AContentTypeManager: IContentTypeManager
    );
  end;

implementation


uses
  StrUtils,
  SysUtils,
  ActiveX,
  UrlMon,
  i_ContentTypeInfo;

const
  CFileNameSeparator = '/';

{ TInternalDomainInfoProviderByDataProvider }

constructor TInternalDomainInfoProviderByDataProvider.Create(
  const AProvider: IConfigDataProvider;
  const AContentTypeManager: IContentTypeManager
);
begin
  inherited Create;
  FProvider := AProvider;
  FContentTypeManager := AContentTypeManager;
end;

function TInternalDomainInfoProviderByDataProvider.LoadBinaryByFilePath(
  const AFilePath: string;
  out AContentType: string
): IBinaryData;
begin
  Result := LoadDataFromSubDataProvider(FProvider, AFilePath, AContentType);
end;

function TInternalDomainInfoProviderByDataProvider.LoadDataFromDataProvider(
  const ADataProvider: IConfigDataProvider;
  const AFileName: string;
  out AContentType: string
): IBinaryData;
var
  VFileName: string;
  VExt: string;
  VContentType: IContentTypeInfoBasic;
  VUrl: WideString;
  VMimeType: PWideChar;
begin
  AContentType := '';
  VFileName := AFileName;
  if VFileName = '' then begin
    VFileName := 'index.html';
  end;
  if AContentType = '' then begin
    VExt := ExtractFileExt(VFileName);
    VContentType := FContentTypeManager.GetInfoByExt(VExt);
    if VContentType <> nil then begin
      AContentType := VContentType.GetContentType;
    end else begin
      VUrl := VFileName;
      if Succeeded(FindMimeFromData(nil, PWideChar(VUrl), nil, 0, nil, 0, VMimeType, 0)) then begin
        try
          AContentType := VMimeType;
        finally
          CoTaskMemFree(VMimeType);
        end;
      end else begin
        AContentType := 'text/html';
      end;
    end;
  end;

  Result := ADataProvider.ReadBinary(VFileName);
end;

function TInternalDomainInfoProviderByDataProvider.LoadDataFromSubDataProvider(
  const ADataProvider: IConfigDataProvider;
  const AFileName: string;
  out AContentType: string
): IBinaryData;
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
      Result := LoadDataFromSubDataProvider(VSubItemProvider, VFileName, AContentType);
    end else begin
      Result := nil;
    end;
  end else begin
    VFileName := AFileName;
    Result := LoadDataFromDataProvider(ADataProvider, VFileName, AContentType);
  end;
end;

end.
