unit u_ThreadExportToCE;

interface

uses
  Types,
  SysUtils,
  Classes,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_VectorItemLonLat,
  i_CoordConverterFactory,
  i_VectorItemsFactory,
  i_MapVersionInfo,
  i_TileStorage,
  u_ResStrings,
  u_ThreadExportAbstract;

type
  TThreadExportToCE = class(TThreadExportAbstract)
  private
    FTileStorage: ITileStorage;
    FVersion: IMapVersionInfo;
    FTargetFile: string;
    FCoordConverterFactory: ICoordConverterFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
    FMaxSize: Integer;
    FComment: string;
    FRecoverInfo: Boolean;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const ACoordConverterFactory: ICoordConverterFactory;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
      const ATargetFile: string;
      const APolygon: ILonLatPolygon;
      const Azoomarr: TByteDynArray;
      const ATileStorage: ITileStorage;
      const AVersion: IMapVersionInfo;
      AMaxSize: Integer;
      const AComment: string;
      ARecoverInfo: Boolean
    );
  end;

implementation

uses
  GR32,
  SAS4WinCE,
  i_TileIterator,
  i_TileInfoBasic,
  i_BinaryData,
  i_VectorItemProjected,
  u_TileIteratorByPolygon;

{ TThreadExportToCE }

constructor TThreadExportToCE.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const ACoordConverterFactory: ICoordConverterFactory;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
  const ATargetFile: string;
  const APolygon: ILonLatPolygon;
  const Azoomarr: TByteDynArray;
  const ATileStorage: ITileStorage;
  const AVersion: IMapVersionInfo;
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
  FCoordConverterFactory := ACoordConverterFactory;
  FProjectionFactory := AProjectionFactory;
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
  VProjectedPolygon: IProjectedPolygon;
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
    VProjectedPolygon :=
      FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
        FProjectionFactory.GetByConverterAndZoom(FTileStorage.CoordConverter, VZoom),
        PolygLL
      );
    VTileIterators[i] := TTileIteratorByPolygon.Create(VProjectedPolygon);
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
          if Supports(FTileStorage.GetTileInfo(VTile, VZoom, FVersion, gtimWithData), ITileInfoWithData, VTileInfo) then begin
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
