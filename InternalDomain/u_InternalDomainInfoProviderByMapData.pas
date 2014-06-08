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

unit u_InternalDomainInfoProviderByMapData;

interface

uses
  Types,
  i_BinaryData,
  i_MapTypeSetChangeable,
  i_TextByVectorItem,
  i_InternalDomainInfoProvider,
  u_BaseInterfacedObject;

type
  TInternalDomainInfoProviderByMapData = class(TBaseInterfacedObject, IInternalDomainInfoProvider)
  private
    FMaps: IMapTypeSetChangeable;
    FVectorDescriptionProvider: ITextByVectorItem;
    FDescriptionSuffix: string;
    function BuildBinaryDataByText(const AText: string): IBinaryData;
    function ParseFilePath(
      const AFilePath: string;
      out AMapGUID: TGUID;
      out AZoom: Byte;
      out ATile: TPoint;
      out AIndex: Integer;
      out ASuffix: string
    ): Boolean;
  private
    function LoadBinaryByFilePath(
      const AFilePath: string;
      out AContentType: string
    ): IBinaryData;
  public
    constructor Create(
      const AMaps: IMapTypeSetChangeable;
      const AVectorDescriptionProvider: ITextByVectorItem;
      const ADescriptionSuffix: string
    );
  end;

implementation

uses
  SysUtils,
  StrUtils,
  c_ZeroGUID,
  i_MapTypes,
  i_TileStorage,
  i_TileInfoBasic,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  u_BinaryData;

const
  CFileNameSeparator = '/';

{ TInternalDomainInfoProviderByMapData }

function TInternalDomainInfoProviderByMapData.BuildBinaryDataByText(
  const AText: string): IBinaryData;
begin
  Result := nil;
  if AText <> '' then begin
    Result := TBinaryData.CreateByString(AText);
  end;
end;

constructor TInternalDomainInfoProviderByMapData.Create(
  const AMaps: IMapTypeSetChangeable;
  const AVectorDescriptionProvider: ITextByVectorItem;
  const ADescriptionSuffix: string
);
begin
  inherited Create;
  FMaps := AMaps;
  FVectorDescriptionProvider := AVectorDescriptionProvider;
  FDescriptionSuffix := ADescriptionSuffix;
end;

function TInternalDomainInfoProviderByMapData.LoadBinaryByFilePath(
  const AFilePath: string; out AContentType: string): IBinaryData;
var
  VMapGUID: TGUID;
  VZoom: Byte;
  VTile: TPoint;
  VIndex: Integer;
  VSuffix: string;
  VMapType: IMapType;
  VTileInfo: ITileInfoBasic;
  VTileInfoWithData: ITileInfoWithData;
  VVectorTile: IVectorItemSubset;
  VItem: IVectorDataItem;
  VText: string;
begin
  Result := nil;
  AContentType := '';
  if not ParseFilePath(AFilePath, VMapGUID, VZoom, VTile, VIndex, VSuffix) then begin
    Exit;
  end;
  VMapType := FMaps.GetStatic.GetMapTypeByGUID(VMapGUID);
  if VMapType = nil then begin
    Exit;
  end;
  if VSuffix = '' then begin
    VTileInfo :=
      VMapType.TileStorage.GetTileInfoEx(
        VTile,
        VZoom,
        VMapType.VersionRequestConfig.GetStatic,
        gtimWithData
      );
    if not Supports(VTileInfo, ITileInfoWithData, VTileInfoWithData) then begin
      Exit;
    end;
    Result := VTileInfoWithData.TileData;
    AContentType := VTileInfoWithData.ContentType.GetContentType;
  end else begin
    if VSuffix = FDescriptionSuffix then begin
      if VMapType.IsKmlTiles then begin
        VVectorTile :=
          VMapType.LoadTileVector(
            VTile,
            VZoom,
            VMapType.VersionRequestConfig.GetStatic,
            True,
            VMapType.CacheVector
          );
        if (VVectorTile <> nil) and (VIndex < VVectorTile.Count) then begin
          VItem := VVectorTile.GetItem(VIndex);
          VText := FVectorDescriptionProvider.GetText(VItem);
          Result := BuildBinaryDataByText(VText);
          AContentType := 'text/html';
        end;
      end;
    end;
  end;
end;

function TInternalDomainInfoProviderByMapData.ParseFilePath(
  const AFilePath: string;
  out AMapGUID: TGUID;
  out AZoom: Byte;
  out ATile: TPoint;
  out AIndex: Integer;
  out ASuffix: string
): Boolean;
var
  VPos: Integer;
  VLastPos: Integer;
  VSubStr: string;
  VZoom: Integer;
begin
  Result := False;
  VLastPos := 0;
  VPos := PosEx(CFileNameSeparator, AFilePath, VLastPos + 1);
  if VPos <= 0 then begin
    VPos := Length(AFilePath);
  end;
  VSubStr := '';
  if VPos > VLastPos then begin
    VSubStr := MidStr(AFilePath, VLastPos + 1, VPos - VLastPos - 1)
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
  VLastPos := VPos;
  VPos := PosEx(CFileNameSeparator, AFilePath, VLastPos + 1);
  if VPos <= 0 then begin
    VPos := Length(AFilePath);
  end;
  VSubStr := '';
  if VPos > VLastPos then begin
    VSubStr := MidStr(AFilePath, VLastPos + 1, VPos - VLastPos - 1)
  end;
  if VSubStr = '' then begin
    Exit;
  end;
  if not TryStrToInt(VSubStr, VZoom) then begin
    Exit;
  end;
  if (VZoom < 0) or (VZoom > 255) then begin
    Exit;
  end;
  AZoom := VZoom;

  VLastPos := VPos;
  VPos := PosEx(CFileNameSeparator, AFilePath, VLastPos + 1);
  if VPos <= 0 then begin
    VPos := Length(AFilePath);
  end;
  VSubStr := '';
  if VPos > VLastPos then begin
    VSubStr := MidStr(AFilePath, VLastPos + 1, VPos - VLastPos - 1)
  end;
  if VSubStr = '' then begin
    Exit;
  end;
  if not TryStrToInt(VSubStr, ATile.X) then begin
    Exit;
  end;

  VLastPos := VPos;
  VPos := PosEx(CFileNameSeparator, AFilePath, VLastPos + 1);
  if VPos <= 0 then begin
    VPos := Length(AFilePath);
  end;
  VSubStr := '';
  if VPos > VLastPos then begin
    VSubStr := MidStr(AFilePath, VLastPos + 1, VPos - VLastPos - 1)
  end;
  if VSubStr = '' then begin
    Exit;
  end;
  if not TryStrToInt(VSubStr, ATile.Y) then begin
    Exit;
  end;

  VLastPos := VPos;
  VPos := PosEx(CFileNameSeparator, AFilePath, VLastPos + 1);
  if VPos <= 0 then begin
    VPos := Length(AFilePath);
  end;
  VSubStr := '';
  if VPos > VLastPos then begin
    VSubStr := MidStr(AFilePath, VLastPos + 1, VPos - VLastPos - 1)
  end;
  if VSubStr = '' then begin
    ASuffix := '';
    AIndex := 0;
    Result := True;
    Exit;
  end;
  if not TryStrToInt(VSubStr, AIndex) then begin
    Exit;
  end;
  VSubStr := MidStr(AFilePath, VPos + 1, Length(AFilePath) - VPos);
  ASuffix := VSubStr;
  Result := True;
end;

end.
