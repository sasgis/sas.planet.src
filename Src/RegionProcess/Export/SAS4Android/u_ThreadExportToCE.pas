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

unit u_ThreadExportToCE;

interface

uses
  Types,
  SysUtils,
  Classes,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_GeometryLonLat,
  i_CoordConverterFactory,
  i_GeometryProjectedFactory,
  i_MapVersionRequest,
  i_TileStorage,
  u_ThreadExportAbstract;

type
  TThreadExportToCE = class(TThreadExportAbstract)
  private
    FTileStorage: ITileStorage;
    FVersion: IMapVersionRequest;
    FTargetFile: string;
    FProjectionSetFactory: IProjectionSetFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FMaxSize: Integer;
    FComment: string;
    FRecoverInfo: Boolean;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AProjectionSetFactory: IProjectionSetFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ATargetFile: string;
      const APolygon: IGeometryLonLatPolygon;
      const Azoomarr: TByteDynArray;
      const ATileStorage: ITileStorage;
      const AVersion: IMapVersionRequest;
      AMaxSize: Integer;
      const AComment: string;
      ARecoverInfo: Boolean
    );
  end;

implementation

uses
  SAS4WinCE,
  i_TileIterator,
  i_TileInfoBasic,
  i_BinaryData,
  i_ProjectionInfo,
  i_GeometryProjected,
  u_TileIteratorByPolygon,
  u_ResStrings;

{ TThreadExportToCE }

constructor TThreadExportToCE.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AProjectionSetFactory: IProjectionSetFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ATargetFile: string;
  const APolygon: IGeometryLonLatPolygon;
  const Azoomarr: TByteDynArray;
  const ATileStorage: ITileStorage;
  const AVersion: IMapVersionRequest;
  AMaxSize: Integer;
  const AComment: string;
  ARecoverInfo: Boolean
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    Azoomarr,
    Self.ClassName
  );
  FTargetFile := ATargetFile;
  FTileStorage := ATileStorage;
  FVersion := AVersion;
  FProjectionSetFactory := AProjectionSetFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FMaxSize := AMaxSize;
  FComment := AComment;
  FRecoverInfo := ARecoverInfo;
end;

procedure TThreadExportToCE.ProcessRegion;
var
  i: integer;
  VExt: string;
  VZoom: Byte;
  VTile: TPoint;
  VTileIterators: array of ITileIterator;
  VTileIterator: ITileIterator;
  VSAS4WinCE: TSAS4WinCE;
  VProjection: IProjectionInfo;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VTileInfo: ITileInfoWithData;
begin
  inherited;
  VTilesToProcess := 0;
  ProgressInfo.SetCaption(SAS_STR_ExportTiles);
  ProgressInfo.SetFirstLine('Preparing tiles to export..');
  SetLength(VTileIterators, Length(FZooms));
  for i := 0 to Length(FZooms) - 1 do begin
    VZoom := FZooms[i];
    VProjection := FTileStorage.ProjectionSet.Zooms[VZoom];
    VProjectedPolygon :=
      FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
        VProjection,
        PolygLL
      );
    VTileIterators[i] := TTileIteratorByPolygon.Create(VProjection, VProjectedPolygon);
    VTilesToProcess := VTilesToProcess + VTileIterators[i].TilesTotal;
    ProgressInfo.SetSecondLine(
      SAS_STR_Zoom + ': ' + inttostr(VZoom) + '  ' + SAS_STR_Tiles + ': ' + inttostr(VTilesToProcess)
    );
  end;

  //Начинает процесс экспорта тайлов в файл fname (без расширения!);
  //maxsize - максимально допустимый размер файлов данных (если <0, то взять
  //значение по умолчанию); cmt - однократно добавляемый в конец файлов комментарий;
  //info - записывать ли в файлы данных дополнительную информацию об
  //тайле (12-15+ байтов) и копирайт в файлы данных и файл индекса.
  //Копирайт является также сигнатурой наличия дополнительной инфы в файлах данных!

  VSAS4WinCE := TSAS4WinCE.Create(FTargetFile, FMaxSize * 1048576, FComment, FRecoverInfo);
  try
    try
      ProgressInfo.SetFirstLine(SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files);
      VTilesProcessed := 0;
      ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
      for i := 0 to Length(FZooms) - 1 do begin
        VZoom := FZooms[i];
        VTileIterator := VTileIterators[i];
        while VTileIterator.Next(VTile) do begin
          if CancelNotifier.IsOperationCanceled(OperationID) then begin
            exit;
          end;
          if Supports(FTileStorage.GetTileInfoEx(VTile, VZoom, FVersion, gtimWithData), ITileInfoWithData, VTileInfo) then begin
            VExt := VTileInfo.ContentType.GetDefaultExt;
            VSAS4WinCE.Add(
              VZoom + 1,
              VTile.X,
              VTile.Y,
              VTileInfo.TileData.Buffer,
              VTileInfo.TileData.Size,
              VExt
            );
          end;
          inc(VTilesProcessed);
          if VTilesProcessed mod 50 = 0 then begin
            ProgressInfo.SetProcessedRatio(VTilesProcessed / VTilesToProcess);
            VExt := '  (.d' + inttostr(VSAS4WinCE.DataNum) + ')';
            if VSAS4WinCE.DataNum < 10 then begin
              VExt := '  (.d0' + inttostr(VSAS4WinCE.DataNum) + ')';
            end;
            if VSAS4WinCE.DataNum < 0 then begin
              VExt := '';
            end;
            ProgressInfo.SetSecondLine(SAS_STR_Processed + ' ' + inttostr(VTilesProcessed) + VExt);
          end;
        end;
      end;
      ProgressInfo.SetFirstLine('Making .inx file ..');
      ProgressInfo.SetSecondLine('');
      VSAS4WinCE.SaveINX(FTargetFile);
    finally
      for i := 0 to Length(FZooms) - 1 do begin
        VTileIterators[i] := nil;
      end;
      VTileIterators := nil;
    end;
  finally
    VSAS4WinCE.Free;
  end;
end;

end.
