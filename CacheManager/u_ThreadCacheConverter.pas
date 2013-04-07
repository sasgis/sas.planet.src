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

unit u_ThreadCacheConverter;

interface

uses
  Classes,
  i_NotifierOperation,
  i_TileInfoBasic,
  i_TileStorage,
  i_CacheConverterProgressInfo,
  u_ThreadCacheManagerAbstract;

type
  TThreadCacheConverter = class(TThreadCacheManagerAbstract)
  private
    FOperationID: Integer;
    FCancelNotifier: INotifierOperation;
    FSourceTileStorage: ITileStorage;
    FSourceStorageRootPath: string;
    FDestTileStorage: ITileStorage;
    FSourceIgnoreTne: Boolean;
    FSourceRemoveTiles: Boolean;
    FDestOverwriteTiles: Boolean;
    FProgressInfo: ICacheConverterProgressInfo;

    function OnSourceTileStorageScan(
      const ATileInfo: TTileInfo
    ): Boolean;
  protected
    procedure Process; override;
  public
    constructor Create(
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const ASourceStorage: ITileStorage;
      const ASourceStorageRootPath: string;
      const ATargetStorage: ITileStorage;
      const ASourceIgnoreTne: Boolean;
      const ASourceRemoveTiles: Boolean;
      const ADestOverwriteTiles: Boolean;
      const AProgressInfo: ICacheConverterProgressInfo
    );
  end;

implementation

uses
  SysUtils;

{ TThreadCacheConverter }

constructor TThreadCacheConverter.Create(
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer;
  const ASourceStorage: ITileStorage;
  const ASourceStorageRootPath: string;
  const ATargetStorage: ITileStorage;
  const ASourceIgnoreTne: Boolean;
  const ASourceRemoveTiles: Boolean;
  const ADestOverwriteTiles: Boolean;
  const AProgressInfo: ICacheConverterProgressInfo
);
begin
  FCancelNotifier := ACancelNotifier;
  FOperationID := AOperationID;
  FSourceIgnoreTne := ASourceIgnoreTne;
  FSourceRemoveTiles := ASourceRemoveTiles;
  FDestOverwriteTiles := ADestOverwriteTiles;
  FProgressInfo := AProgressInfo;
  FSourceTileStorage := ASourceStorage;
  FSourceStorageRootPath := ASourceStorageRootPath;
  FDestTileStorage := ATargetStorage;

  inherited Create(FCancelNotifier, FOperationID, AnsiString(Self.ClassName));
end;

procedure TThreadCacheConverter.Process;
var
  VEnum: IEnumTileInfo;
  VTileInfo: TTileInfo;
begin
  VEnum := FSourceTileStorage.ScanTiles(FSourceIgnoreTne, not FDestTileStorage.GetIsCanSaveMultiVersionTiles);
  while VEnum.Next(VTileInfo) do begin
    if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
      Break;
    end;
    if not OnSourceTileStorageScan(VTileInfo) then begin
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
  VTileFullPath: string;
begin
  Result := False;
  if not FCancelNotifier.IsOperationCanceled(FOperationID) then begin

    if not FDestOverwriteTiles then begin
      VTileInfo :=
        FDestTileStorage.GetTileInfo(
          ATileInfo.FTile,
          ATileInfo.FZoom,
          ATileInfo.FVersionInfo,
          gtimAsIs
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
          ATileInfo.FVersionInfo,
          ATileInfo.FLoadDate,
          ATileInfo.FContentType,
          ATileInfo.FData
        );
        Result := True;
      end else if ATileInfo.FInfoType = titTneExists then begin
        FDestTileStorage.SaveTNE(
          ATileInfo.FTile,
          ATileInfo.FZoom,
          ATileInfo.FVersionInfo,
          ATileInfo.FLoadDate
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

    VTileFullPath :=
      FSourceTileStorage.GetTileFileName(
        ATileInfo.FTile,
        ATileInfo.FZoom,
        ATileInfo.FVersionInfo
      );

    FProgressInfo.LastTileName :=
      StringReplace(VTileFullPath, FSourceStorageRootPath, '', [rfIgnoreCase]);
  end;
end;

end.
