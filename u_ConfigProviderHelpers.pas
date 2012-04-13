{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_ConfigProviderHelpers;

interface

uses
  GR32,
  i_ContentTypeManager,
  i_Bitmap32Static,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider;

procedure WriteColor32(
  const AConfigProvider: IConfigDataWriteProvider;
  const AIdent: string;
  AValue: TColor32
);
function ReadColor32(
  const AConfigProvider: IConfigDataProvider;
  const AIdent: string;
  ADefault: TColor32
): TColor32;

function ReadBitmapByFileRef(
  const AConfigProvider: IConfigDataProvider;
  const AFullFileName: string;
  const AContentTypeManager: IContentTypeManager;
  const ADefault: IBitmap32Static
): IBitmap32Static;

implementation

uses
  SysUtils,
  Graphics,
  i_BinaryData,
  i_ContentTypeInfo;

function ReadColor32(
  const AConfigProvider: IConfigDataProvider;
  const AIdent: string;
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
  const AConfigProvider: IConfigDataWriteProvider;
  const AIdent: string;
  AValue: TColor32
);
begin
  AConfigProvider.WriteString(AIdent + 'Hex', HexDisplayPrefix + IntToHex(AValue, 8));
end;

function ReadBitmapByFileRef(
  const AConfigProvider: IConfigDataProvider;
  const AFullFileName: string;
  const AContentTypeManager: IContentTypeManager;
  const ADefault: IBitmap32Static
): IBitmap32Static;
var
  VFilePath: string;
  VFileName: string;
  VFileExt: string;
  VResourceProvider: IConfigDataProvider;
  VInfoBasic: IContentTypeInfoBasic;
  VBitmapContntType: IContentTypeInfoBitmap;
  VData: IBinaryData;
begin
  Result := ADefault;
  VFilePath := ExcludeTrailingPathDelimiter(ExtractFilePath(AFullFileName));
  VFileName := ExtractFileName(AFullFileName);
  VFileExt := ExtractFileExt(VFileName);

  try
    VResourceProvider := AConfigProvider.GetSubItem(VFilePath);
  except
    Assert(False, 'Ошибка при получении пути ' + VFilePath);
  end;

  if VResourceProvider <> nil then begin
    VData := VResourceProvider.ReadBinary(VFileName);
    if VData <> nil then begin
      VInfoBasic := AContentTypeManager.GetInfoByExt(VFileExt);
      if VInfoBasic <> nil then begin
        if Supports(VInfoBasic, IContentTypeInfoBitmap, VBitmapContntType) then begin
          try
            Result := VBitmapContntType.GetLoader.Load(VData);
          except
            Assert(False, 'Ошибка при загрузке картинки ' + AFullFileName);
          end;
        end;
      end;
    end;
  end;
end;

end.
