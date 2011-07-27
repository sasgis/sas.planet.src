unit u_ConfigProviderHelpers;

interface

uses
  GR32,
  i_ContentTypeManager,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider;

procedure WriteColor32(
  AConfigProvider: IConfigDataWriteProvider;
  AIdent: string;
  AValue: TColor32
);
function ReadColor32(
  AConfigProvider: IConfigDataProvider;
  AIdent: string;
  ADefault: TColor32
): TColor32;

function ReadBitmapByFileRef(
  AConfigProvider: IConfigDataProvider;
  AIdent: string;
  ADefFileName: string;
  AContentTypeManager: IContentTypeManager;
  ABitmap: TCustomBitmap32
): string;

implementation

uses
  Classes,
  SysUtils,
  Graphics,
  i_ContentTypeInfo;

function ReadColor32(
  AConfigProvider: IConfigDataProvider;
  AIdent: string;
  ADefault: TColor32
): TColor32;
var
  VColor: TColor;
  VAlfa: Integer;
  VHexString: string;
  VIntColor: Integer;
begin
  Result := ADefault;
  if AConfigProvider <> nil then begin
    VHexString := AConfigProvider.ReadString(AIdent + 'Hex', '');
    if VHexString = '' then begin
      VAlfa := AlphaComponent(Result);
      VColor := WinColor(Result);
      VAlfa := AConfigProvider.ReadInteger(AIdent + 'Alfa', VAlfa);
      VColor := AConfigProvider.ReadInteger(AIdent, VColor);
      Result := SetAlpha(Color32(VColor), VAlfa);
    end else begin
      if TryStrToInt(VHexString, VIntColor) then begin
        Result := VIntColor;
      end;
    end;
  end;
end;

procedure WriteColor32(
  AConfigProvider: IConfigDataWriteProvider;
  AIdent: string;
  AValue: TColor32
);
begin
  AConfigProvider.WriteString(AIdent + 'Hex', HexDisplayPrefix + IntToHex(AValue, 8));
end;

function ReadBitmapByFileRef(
  AConfigProvider: IConfigDataProvider;
  AIdent: string;
  ADefFileName: string;
  AContentTypeManager: IContentTypeManager;
  ABitmap: TCustomBitmap32
): string;
var
  VFileName: string;
  VPath: string;
  VName: string;
  VExt: string;
  VResourceProvider: IConfigDataProvider;
  VStream: TMemoryStream;
  VInfoBasic: IContentTypeInfoBasic;
  VBitmapContntType: IContentTypeInfoBitmap;
begin
  Result := ADefFileName;

  VFileName := AConfigProvider.ReadString(AIdent, ADefFileName);
  VPath := ExcludeTrailingPathDelimiter(ExtractFilePath(VFileName));
  VName := ExtractFileName(VFileName);
  VExt := ExtractFileExt(VName);

  try
    VResourceProvider := AConfigProvider.GetSubItem(VPath);
  except
    Assert(False, 'Ошибка при получении пути ' + VPath);
  end;

  if VResourceProvider <> nil then begin
    VStream := TMemoryStream.Create;
    try
      if VResourceProvider.ReadBinaryStream(VName, VStream) > 0 then begin
        VInfoBasic := AContentTypeManager.GetInfoByExt(VExt);
        if VInfoBasic <> nil then begin
          if Supports(VInfoBasic, IContentTypeInfoBitmap, VBitmapContntType) then begin
            VStream.Position := 0;
            try
              VBitmapContntType.GetLoader.LoadFromStream(VStream, ABitmap);
              Result := VFileName;
            except
              Assert(False, 'Ошибка при загрузке картинки ' + VFileName);
            end;
          end;
        end;
      end;
    finally
      VStream.Free;
    end;
  end;
end;

end.
