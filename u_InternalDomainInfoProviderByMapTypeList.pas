{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_InternalDomainInfoProviderByMapTypeList;

interface

uses
  i_BinaryData,
  i_ConfigDataProvider,
  i_ZmpInfo,
  i_ZmpInfoSet,
  i_ContentTypeManager,
  i_InternalDomainInfoProvider;

type
  TInternalDomainInfoProviderByMapTypeList = class(TInterfacedObject, IInternalDomainInfoProvider)
  private
    FZmpInfoSet: IZmpInfoSet;
    FContentTypeManager: IContentTypeManager;
    function ParseFilePath(AFilePath: string; out AZmpGUID: TGUID; out AFileName: string): Boolean;
    function LoadStreamFromZmp(AZmp: IZmpInfo; AFileName: string; out AContentType: string): IBinaryData;
    function LoadStreamFromDataProvider(ADataProvider: IConfigDataProvider; AFileName: string; out AContentType: string): IBinaryData;
    function LoadStreamFromSubDataProvider(ADataProvider: IConfigDataProvider; AFileName: string; out AContentType: string): IBinaryData;
  protected
    function LoadBinaryByFilePath(AFilePath: string; out AContentType: string): IBinaryData;
  public
    constructor Create(
      AZmpInfoSet: IZmpInfoSet;
      AContentTypeManager: IContentTypeManager
    );
  end;

implementation

uses
  StrUtils,
  SysUtils,
  i_ContentTypeInfo,
  c_ZeroGUID;

const
  CFileNameSeparator = '/';

{ TInternalDomainInfoProviderByMapTypeList }

constructor TInternalDomainInfoProviderByMapTypeList.Create(
  AZmpInfoSet: IZmpInfoSet;
  AContentTypeManager: IContentTypeManager
);
begin
  FZmpInfoSet := AZmpInfoSet;
  FContentTypeManager := AContentTypeManager;
end;

function TInternalDomainInfoProviderByMapTypeList.LoadBinaryByFilePath(
  AFilePath: string;
  out AContentType: string
): IBinaryData;
var
  VGuid: TGUID;
  VZmp: IZmpInfo;
  VFileName: string;
begin
  Result := nil;
  if ParseFilePath(AFilePath, VGuid, VFileName) then begin
    VZmp := FZmpInfoSet.GetZmpByGUID(VGuid);
    if VZmp <> nil then begin
      Result := LoadStreamFromZmp(VZmp, VFileName, AContentType);
    end;
  end;
end;

function TInternalDomainInfoProviderByMapTypeList.LoadStreamFromDataProvider(
  ADataProvider: IConfigDataProvider;
  AFileName: string;
  out AContentType: string
): IBinaryData;
var
  VFileName: string;
  VExt: string;
  VContentType: IContentTypeInfoBasic;
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
      AContentType := 'text/html'
    end;
  end;

  Result := ADataProvider.ReadBinary(VFileName);
end;

function TInternalDomainInfoProviderByMapTypeList.LoadStreamFromSubDataProvider(
  ADataProvider: IConfigDataProvider;
  AFileName: string;
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
      Result := LoadStreamFromSubDataProvider(VSubItemProvider, VFileName, AContentType);
    end else begin
      Result := nil;
    end;
  end else begin
    VFileName := AFileName;
    Result := LoadStreamFromDataProvider(ADataProvider, VFileName, AContentType);
  end;
end;

function TInternalDomainInfoProviderByMapTypeList.LoadStreamFromZmp(
  AZmp: IZmpInfo;
  AFileName: string;
  out AContentType: string
): IBinaryData;
begin
  Result := LoadStreamFromSubDataProvider(AZmp.DataProvider, AFileName, AContentType);
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
