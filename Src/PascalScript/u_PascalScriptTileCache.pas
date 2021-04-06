{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit u_PascalScriptTileCache;

interface

uses
  Types,
  i_TileStorage,
  i_MapVersionFactory,
  i_ContentTypeManager,
  i_PascalScriptTileCache,
  u_BaseInterfacedObject;

type
  TPascalScriptTileCache = class(TBaseInterfacedObject, IPascalScriptTileCache)
  private
    FStorage: ITileStorage;
    FMapVersionFactory: IMapVersionFactory;
    FContentTypeManager: IContentTypeManager;
  private
    { IPascalScriptTileCache }
    function Read(
      const X: Integer;
      const Y: Integer;
      const AZoom: Byte;
      const AVersion: string;
      const AWithData: Boolean
    ): TPascalScriptTileInfo;

    function Write(
      const X: Integer;
      const Y: Integer;
      const AZoom: Byte;
      const AVersion: string;
      const AContentType: AnsiString;
      const AData: AnsiString;
      const AIsOverwrite: Boolean
    ): Boolean;

    function WriteTne(
      const X: Integer;
      const Y: Integer;
      const AZoom: Byte;
      const AVersion: string
    ): Boolean;

    function Delete(
      const X: Integer;
      const Y: Integer;
      const AZoom: Byte;
      const AVersion: string
    ): Boolean;
  public
    constructor Create(
      const AStorage: ITileStorage;
      const AMapVersionFactory: IMapVersionFactory;
      const AContentTypeManager: IContentTypeManager
    );
  end;

implementation

uses
  SysUtils,
  DateUtils,
  i_BinaryData,
  i_TileInfoBasic,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  u_BinaryData;

const
  CEmptyTileInfo: TPascalScriptTileInfo = (
    IsExists    : False;
    IsExistsTne : False;
    LoadDate    : 0;
    Size        : 0;
    Version     : '';
    ContentType : '';
    Data        : '';
  );

{ TPascalScriptTileCache }

constructor TPascalScriptTileCache.Create(
  const AStorage: ITileStorage;
  const AMapVersionFactory: IMapVersionFactory;
  const AContentTypeManager: IContentTypeManager
);
begin
  inherited Create;

  FStorage := AStorage;
  FMapVersionFactory := AMapVersionFactory;
  FContentTypeManager := AContentTypeManager;
end;

function TPascalScriptTileCache.Read(
  const X: Integer;
  const Y: Integer;
  const AZoom: Byte;
  const AVersion: string;
  const AWithData: Boolean
): TPascalScriptTileInfo;
var
  VMode: TGetTileInfoMode;
  VVersion: IMapVersionInfo;
  VInfo: ITileInfoBasic;
  VInfoWithData: ITileInfoWithData;
  VData: IBinaryData;
begin
  Result := CEmptyTileInfo;

  if AWithData then begin
    VMode := gtimWithData;
  end else begin
    VMode := gtimWithoutData;
  end;

  VVersion := FMapVersionFactory.CreateByStoreString(AVersion);
  VInfo := FStorage.GetTileInfo(Point(X, Y), AZoom, VVersion, VMode);

  if VInfo = nil then begin
    Exit;
  end;

  Result.IsExists := VInfo.IsExists;
  Result.IsExistsTne := VInfo.IsExistsTNE;
  Result.LoadDate := DateTimeToUnix(VInfo.LoadDate);
  Result.Size := VInfo.Size;
  if VInfo.VersionInfo <> nil then begin
    Result.Version := VInfo.VersionInfo.StoreString;
  end;
  if VInfo.ContentType <> nil then begin
    Result.ContentType := VInfo.ContentType.GetContentType;
  end;

  if AWithData and Supports(VInfo, ITileInfoWithData, VInfoWithData) then begin
    VData := VInfoWithData.TileData;
    if VData.Size > 0 then begin
      SetLength(Result.Data, VData.Size);
      Move(VData.Buffer^, Result.Data[1], VData.Size);
    end;
  end;
end;

function TPascalScriptTileCache.Write(
  const X: Integer;
  const Y: Integer;
  const AZoom: Byte;
  const AVersion: string;
  const AContentType: AnsiString;
  const AData: AnsiString;
  const AIsOverwrite: Boolean
): Boolean;
var
  VData: IBinaryData;
  VVersion: IMapVersionInfo;
  VContentType: IContentTypeInfoBasic;
begin
  VData := TBinaryData.CreateByAnsiString(AData);
  VVersion := FMapVersionFactory.CreateByStoreString(AVersion);
  VContentType := FContentTypeManager.GetInfo(AContentType);

  if VContentType = nil then begin
    raise Exception.Create('Unknown Content-Type: ' + string(AContentType));
  end;

  Result := FStorage.SaveTile(Point(X, Y), AZoom, VVersion, Now, VContentType,
    VData, AIsOverwrite);
end;

function TPascalScriptTileCache.WriteTne(
  const X: Integer;
  const Y: Integer;
  const AZoom: Byte;
  const AVersion: string
): Boolean;
var
  VVersion: IMapVersionInfo;
begin
  VVersion := FMapVersionFactory.CreateByStoreString(AVersion);
  Result := FStorage.SaveTile(Point(X, Y), AZoom, VVersion, Now, nil, nil, True);
end;

function TPascalScriptTileCache.Delete(
  const X: Integer;
  const Y: Integer;
  const AZoom: Byte;
  const AVersion: string
): Boolean;
var
  VVersion: IMapVersionInfo;
begin
  VVersion := FMapVersionFactory.CreateByStoreString(AVersion);
  Result := FStorage.DeleteTile(Point(X, Y), AZoom, VVersion);
end;

end.
