{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

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
      out AContentType: AnsiString
    ): IBinaryData;
    function LoadDataFromDataProvider(
      const ADataProvider: IConfigDataProvider;
      const AFileName: string;
      out AContentType: AnsiString
    ): IBinaryData;
  private
    function LoadBinaryByFilePath(
      const AFilePath: string;
      out AContentType: AnsiString
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
  {$IFNDef UNICODE}
  Compatibility,
  {$ENDIF}
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
  out AContentType: AnsiString
): IBinaryData;
begin
  Result := LoadDataFromSubDataProvider(FProvider, AFilePath, AContentType);
end;

function TInternalDomainInfoProviderByDataProvider.LoadDataFromDataProvider(
  const ADataProvider: IConfigDataProvider;
  const AFileName: string;
  out AContentType: AnsiString
): IBinaryData;
var
  VFileName: string;
  VExt: AnsiString;
  VContentType: IContentTypeInfoBasic;
  VUrl: UnicodeString;
  VMimeType: PWideChar;
begin
  AContentType := '';
  VFileName := AFileName;
  if VFileName = '' then begin
    VFileName := 'index.html';
  end;
  if AContentType = '' then begin
    VExt := AnsiString(ExtractFileExt(VFileName));
    VContentType := FContentTypeManager.GetInfoByExt(VExt);
    if VContentType <> nil then begin
      AContentType := VContentType.GetContentType;
    end else begin
      VUrl := VFileName;
      if Succeeded(FindMimeFromData(nil, PWideChar(VUrl), nil, 0, nil, 0, VMimeType, 0)) then begin
        try
          AContentType := AnsiString(VMimeType);
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
  out AContentType: AnsiString
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
