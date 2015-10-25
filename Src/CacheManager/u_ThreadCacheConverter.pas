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

unit u_ThreadCacheConverter;

interface

uses
  Classes,
  i_NotifierOperation,
  i_TileInfoBasic,
  i_TileStorage,
  i_MapVersionInfo,
  i_CacheConverterProgressInfo,
  u_ThreadCacheManagerAbstract;

type
  TThreadCacheConverter = class(TThreadCacheManagerAbstract)
  private
    FOperationID: Integer;
    FCancelNotifier: INotifierOperation;
    FSourceTileStorage: ITileStorage;
    FSourceVersionInfo: IMapVersionInfo;
    FSourceStorageRootPath: string;
    FDestTileStorage: ITileStorage;
    FDestVersionInfo: IMapVersionInfo;
    FSourceIgnoreTne: Boolean;
    FSourceRemoveTiles: Boolean;
    FDestOverwriteTiles: Boolean;
    FProgressInfo: ICacheConverterProgressInfo;
    FCheckSourceVersion: Boolean;
    FReplaceDestVersion: Boolean;
    FIgnoreMultiVersionTiles: Boolean;

    procedure SetLastTileName(const ATileInfo: TTileInfo); inline;
    function OnSourceTileStorageScan(const ATileInfo: TTileInfo): Boolean;
  protected
    procedure Process; override;
  public
    constructor Create(
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const ASourceStorage: ITileStorage;
      const ASourceVersionInfo: IMapVersionInfo;
      const ASourceStorageRootPath: string;
      const ADestStorage: ITileStorage;
      const ADestVersionInfo: IMapVersionInfo;
      const ASourceIgnoreTne: Boolean;
      const ASourceRemoveTiles: Boolean;
      const ADestOverwriteTiles: Boolean;
      const AProgressInfo: ICacheConverterProgressInfo
    );
  end;

implementation

uses
  SysUtils,
  i_TileStorageAbilities;

{ TThreadCacheConverter }

constructor TThreadCacheConverter.Create(
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer;
  const ASourceStorage: ITileStorage;
  const ASourceVersionInfo: IMapVersionInfo;
  const ASourceStorageRootPath: string;
  const ADestStorage: ITileStorage;
  const ADestVersionInfo: IMapVersionInfo;
  const ASourceIgnoreTne: Boolean;
  const ASourceRemoveTiles: Boolean;
  const ADestOverwriteTiles: Boolean;
  const AProgressInfo: ICacheConverterProgressInfo
);
begin
  Assert(Assigned(ACancelNotifier));
  Assert(Assigned(ASourceStorage));
  Assert(Assigned(ADestStorage));
  Assert(Assigned(AProgressInfo));
  FCancelNotifier := ACancelNotifier;
  FOperationID := AOperationID;
  FSourceIgnoreTne := ASourceIgnoreTne;
  FSourceRemoveTiles := ASourceRemoveTiles;
  FDestOverwriteTiles := ADestOverwriteTiles;
  FProgressInfo := AProgressInfo;
  FSourceTileStorage := ASourceStorage;
  FSourceVersionInfo := ASourceVersionInfo;
  FSourceStorageRootPath := ASourceStorageRootPath;
  FDestTileStorage := ADestStorage;
  FDestVersionInfo := ADestVersionInfo;

  FCheckSourceVersion := Assigned(FSourceVersionInfo);
  FReplaceDestVersion := Assigned(FDestVersionInfo);

  if FSourceTileStorage.StorageTypeAbilities.VersionSupport = tstvsMultiVersions then begin
    if FCheckSourceVersion then begin
      // process all versions
      FIgnoreMultiVersionTiles := False;
    end else begin
      // process all versions if dest storage support versioned tiles
      FIgnoreMultiVersionTiles := not (FDestTileStorage.StorageTypeAbilities.VersionSupport = tstvsMultiVersions);
    end;
  end else begin
    // no versioned tiles in source storage
    FIgnoreMultiVersionTiles := True;
  end;

  inherited Create(FCancelNotifier, FOperationID, Self.ClassName);
end;

procedure TThreadCacheConverter.Process;
var
  VEnum: IEnumTileInfo;
  VTileInfo: TTileInfo;
begin
  VEnum := FSourceTileStorage.ScanTiles(FSourceIgnoreTne, FIgnoreMultiVersionTiles);
  while VEnum.Next(VTileInfo) do begin
    if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
      Break;
    end;
    if FCheckSourceVersion then begin
      if Assigned(VTileInfo.FVersionInfo) and WideSameText(VTileInfo.FVersionInfo.StoreString, FSourceVersionInfo.StoreString) then begin
        if not OnSourceTileStorageScan(VTileInfo) then begin
          Break;
        end;
      end else begin
        SetLastTileName(VTileInfo);
        FProgressInfo.TilesSkipped := FProgressInfo.TilesSkipped + 1;
      end;
    end else if not OnSourceTileStorageScan(VTileInfo) then begin
      Break;
    end;
    if FSourceRemoveTiles then begin
      if VTileInfo.FInfoType in [titExists, titTneExists] then begin
        FSourceTileStorage.DeleteTile(VTileInfo.FTile, VTileInfo.FZoom, VTileInfo.FVersionInfo);
      end;
    end;
  end;
  FProgressInfo.Finished := True;
end;

function TThreadCacheConverter.OnSourceTileStorageScan(
  const ATileInfo: TTileInfo
): Boolean;
var
  VTileInfo: ITileInfoBasic;
  VDestVersionInfo: IMapVersionInfo;
begin
  Result := False;
  if not FCancelNotifier.IsOperationCanceled(FOperationID) then begin

    if FReplaceDestVersion then begin
      VDestVersionInfo := FDestVersionInfo;
    end else begin
      VDestVersionInfo := ATileInfo.FVersionInfo;
    end;

    if not FDestOverwriteTiles then begin
      VTileInfo :=
        FDestTileStorage.GetTileInfo(
          ATileInfo.FTile,
          ATileInfo.FZoom,
          VDestVersionInfo,
          gtimWithoutData
        );
      if Assigned(VTileInfo) then begin
        if (VTileInfo.IsExists or (VTileInfo.IsExistsTNE and (ATileInfo.FInfoType = titTneExists))) then begin
          Result := True;
          FProgressInfo.TilesSkipped := FProgressInfo.TilesSkipped + 1;
        end;
      end;
    end;

    if not Result then begin

      if ATileInfo.FInfoType = titExists then begin
        FDestTileStorage.SaveTile(
          ATileInfo.FTile,
          ATileInfo.FZoom,
          VDestVersionInfo,
          ATileInfo.FLoadDate,
          ATileInfo.FContentType,
          ATileInfo.FData,
          True
        );
        Result := True;
      end else if ATileInfo.FInfoType = titTneExists then begin
        FDestTileStorage.SaveTile(
          ATileInfo.FTile,
          ATileInfo.FZoom,
          VDestVersionInfo,
          ATileInfo.FLoadDate,
          nil,
          nil,
          True
        );
        Result := True;
      end;

      if Result then begin
        FProgressInfo.TilesProcessed := FProgressInfo.TilesProcessed + 1;
        if Assigned(ATileInfo.FData) then begin
          FProgressInfo.TilesSize := FProgressInfo.TilesSize + ATileInfo.FData.Size;
        end;
      end;
    end;

    SetLastTileName(ATileInfo);
  end;
end;

procedure TThreadCacheConverter.SetLastTileName(const ATileInfo: TTileInfo);
var
  VTileFullPath: string;
begin
  VTileFullPath :=
    FSourceTileStorage.GetTileFileName(
      ATileInfo.FTile,
      ATileInfo.FZoom,
      ATileInfo.FVersionInfo
    );
  FProgressInfo.LastTileName :=
    StringReplace(VTileFullPath, FSourceStorageRootPath, '', [rfIgnoreCase]);
end;

end.
