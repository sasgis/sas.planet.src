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

unit u_InternalDomainInfoProviderByTileStorageOptions;

interface

uses
  i_BinaryData,
  i_MapTypeSetChangeable,
  i_InternalDomainInfoProvider,
  u_BaseInterfacedObject;

type
  TInternalDomainInfoProviderByTileStorageOptions = class(TBaseInterfacedObject, IInternalDomainInfoProvider)
  private
    FMaps: IMapTypeSetChangeable;

    function BuildBinaryDataByText(const AText: string): IBinaryData;

    procedure DecorateHtml(
      const AHtmlTitle: String;
      var AHtmlText: String
    );

    function ParseFilePath(
      const AFilePath: string;
      out AMapGUID: TGUID;
      out AFullRequestPrefix: String;
      out ARequest: String
    ): Boolean;
  private
    function LoadBinaryByFilePath(
      const AFilePath: string;
      out AContentType: AnsiString
    ): IBinaryData;
  public
    constructor Create(
      const AMaps: IMapTypeSetChangeable
    );
  end;

implementation

uses
  SysUtils,
  StrUtils,
  c_ZeroGUID,
  c_InternalBrowser,
  i_MapType,
  i_TileStorage,
  i_InternalDomainOptions,
  u_BinaryData;

const
  CFileNameSeparator = '/';

{ TInternalDomainInfoProviderByTileStorageOptions }

function TInternalDomainInfoProviderByTileStorageOptions.BuildBinaryDataByText(
  const AText: string): IBinaryData;
begin
  Result := nil;
  if AText <> '' then begin
    Result := TBinaryData.CreateByString(AText);
  end;
end;

constructor TInternalDomainInfoProviderByTileStorageOptions.Create(
  const AMaps: IMapTypeSetChangeable
);
begin
  inherited Create;
  FMaps := AMaps;
end;

procedure TInternalDomainInfoProviderByTileStorageOptions.DecorateHtml(
  const AHtmlTitle: String;
  var AHtmlText: String
);
begin
  AHtmlText := '<html><head><title>' + AHtmlTitle + '</title></head><body>' + AHtmlText + '</body></html>';
end;

function TInternalDomainInfoProviderByTileStorageOptions.LoadBinaryByFilePath(
  const AFilePath: string;
  out AContentType: AnsiString
): IBinaryData;
var
  VMapGUID: TGUID;
  VFullRequestPrefix: String;
  VRequest, VResponse: String;
  VResponseFlags: TDomainOptionsResponseFlags;
  VMapType: IMapType;
  VTileStorage: ITileStorage;
  VInternalDomainOptions: IInternalDomainOptions;
begin
  Result := nil;

  // sas://TileStorageConfig/{9A360A51-7A72-402D-8A12-D670BD739B7B}

  // always
  AContentType := 'text/html';

  // parse request
  if not ParseFilePath(AFilePath, VMapGUID, VFullRequestPrefix, VRequest) then begin
    Exit;
  end;

  VMapType := FMaps.GetStatic.GetMapTypeByGUID(VMapGUID);
  if VMapType = nil then begin
    // unknown map
    Exit;
  end;

  VTileStorage := VMapType.TileStorage;

  if VTileStorage = nil then begin
    // no storage
    Exit;
  end;

  VFullRequestPrefix := CTileStorageOptionsInternalURL + VFullRequestPrefix;
  VResponseFlags := [];

  while (Length(VRequest) > 0) and (VRequest[1] = '/') do begin
    System.Delete(VRequest, 1, 1);
  end;

  if Supports(VTileStorage, IInternalDomainOptions, VInternalDomainOptions) then begin
    if VInternalDomainOptions.DomainHtmlOptions(VFullRequestPrefix, VRequest, VResponse, VResponseFlags) then begin
      // ok
    end else begin
      // failed to execute
      VResponse := 'Failed to execute';
    end;
  end else begin
    // not implemented
    VResponse := 'Not implemented';
  end;

  if not (dorf_HtmlDecorated in VResponseFlags) then begin
    DecorateHtml(VRequest, VResponse);
  end;

  if (dorf_ClearMemCache in VResponseFlags) then begin
    VMapType.ClearMemCache;
  end;

  Result := BuildBinaryDataByText(VResponse);
end;

function TInternalDomainInfoProviderByTileStorageOptions.ParseFilePath(
  const AFilePath: string;
  out AMapGUID: TGUID;
  out AFullRequestPrefix: String;
  out ARequest: String
): Boolean;
var
  VPos: Integer;
  VLastPos: Integer;
  VSubStr: string;
begin
  Result := False;
  VLastPos := 0;
  VPos := PosEx(CFileNameSeparator, AFilePath, VLastPos + 1);
  if VPos <= 0 then begin
    VPos := Length(AFilePath) + 1;
  end;
  VSubStr := '';
  if VPos > VLastPos then begin
    VSubStr := MidStr(AFilePath, VLastPos + 1, VPos - VLastPos - 1);
  end;
  if VSubStr = '' then begin
    Exit;
  end;
  try
    AMapGUID := StringToGUID(VSubStr);
  except
    AMapGUID := CGUID_Zero;
  end;
  if IsEqualGUID(AMapGUID, CGUID_Zero) then begin
    Exit;
  end;

  AFullRequestPrefix := System.Copy(AFilePath, 1, VPos);
  ARequest := System.Copy(AFilePath, VPos + 1, Length(AFilePath));
  Result := True;
end;

end.
