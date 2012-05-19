unit u_ThreadExportToCE;

interface

uses
  SysUtils,
  Classes,
  i_OperationNotifier,
  i_RegionProcessProgressInfo,
  i_VectorItemLonLat,
  i_CoordConverterFactory,
  i_VectorItmesFactory,
  u_MapType,
  u_ResStrings,
  u_ThreadExportAbstract;

type
  TThreadExportToCE = class(TThreadExportAbstract)
  private
    FMapType: TMapType;
    FTargetFile: string;
    FCoordConverterFactory: ICoordConverterFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FMaxSize: Integer;
    FComment: string;
    FRecoverInfo: Boolean;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      const AProgressInfo: IRegionProcessProgressInfo;
      const ACoordConverterFactory: ICoordConverterFactory;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory;
      const ATargetFile: string;
      const APolygon: ILonLatPolygon;
      const Azoomarr: array of boolean;
      AMapType: TMapType;
      AMaxSize: Integer;
      const AComment: string;
      ARecoverInfo: Boolean
    );
  end;

implementation

uses
  Types,
  GR32,
  SAS4WinCE,
  i_TileIterator,
  i_TileInfoBasic,
  i_BinaryData,
  i_VectorItemProjected,
  u_TileIteratorByPolygon,
  u_TileStorageAbstract,
  u_StreamReadOnlyByBinaryData;

{ TThreadExportToCE }

constructor TThreadExportToCE.Create(
  const ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  const AProgressInfo: IRegionProcessProgressInfo;
  const ACoordConverterFactory: ICoordConverterFactory;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
  const ATargetFile: string;
  const APolygon: ILonLatPolygon;
  const Azoomarr: array of boolean;
  AMapType: TMapType;
  AMaxSize: Integer;
  const AComment: string;
  ARecoverInfo: Boolean
);
begin
  inherited Create(
    ACancelNotifier,
    AOperationID,
    AProgressInfo,
    APolygon,
    Azoomarr
  );
  FTargetFile := ATargetFile;
  FMapType := AMapType;
  FCoordConverterFactory := ACoordConverterFactory;
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
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
  VTileStorage: TTileStorageAbstract;
  VSAS4WinCE: TSAS4WinCE;
  VProjectedPolygon: IProjectedPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VData: IBinaryData;
  VTileInfo: ITileInfoBasic;
begin
  inherited;
  VTilesToProcess := 0;
  ProgressInfo.Caption := SAS_STR_ExportTiles;
  ProgressInfo.FirstLine := 'Preparing tiles to export..';
  SetLength(VTileIterators, Length(FZooms));
  for i := 0 to Length(FZooms) - 1 do begin
    VZoom := FZooms[i];
    VProjectedPolygon :=
      FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
        FProjectionFactory.GetByConverterAndZoom(FMapType.GeoConvert, VZoom),
        PolygLL
      );
    VTileIterators[i] := TTileIteratorByPolygon.Create(VProjectedPolygon);
    VTilesToProcess := VTilesToProcess + VTileIterators[i].TilesTotal;
    ProgressInfo.SecondLine := SAS_STR_Zoom + ': ' + inttostr(Vzoom) + '  ' + SAS_STR_Tiles + ': ' + inttostr(VTilesToProcess);
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
      ProgressInfo.FirstLine := SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files;
      VTileStorage := FMapType.TileStorage;
      VTilesProcessed := 0;
      ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
      for i := 0 to Length(FZooms) - 1 do begin
        VZoom := FZooms[i];
        VTileIterator := VTileIterators[i];
        while VTileIterator.Next(VTile) do begin
          if CancelNotifier.IsOperationCanceled(OperationID) then begin
            exit;
          end;
          VExt := FMapType.StorageConfig.TileFileExt;
          VData := VTileStorage.LoadTile(VTile, VZoom, nil, VTileInfo);
          if VData <> nil then begin
            VSAS4WinCE.Add(
              VZoom + 1,
              VTile.X,
              VTile.Y,
              VData.Buffer,
              VData.Size,
              VExt
            );
          end;
          inc(VTilesProcessed);
          if VTilesProcessed mod 50 = 0 then begin
            ProgressInfo.Processed := VTilesProcessed / VTilesToProcess;
            VExt := '  (.d' + inttostr(VSAS4WinCE.DataNum) + ')';
            if VSAS4WinCE.DataNum < 10 then begin
              VExt := '  (.d0' + inttostr(VSAS4WinCE.DataNum) + ')';
            end;
            if VSAS4WinCE.DataNum < 0 then begin
              VExt := '';
            end;
            ProgressInfo.SecondLine := SAS_STR_Processed + ' ' + inttostr(VTilesProcessed) + VExt;
          end;
        end;
      end;
      ProgressInfo.FirstLine := 'Making .inx file ..';
      ProgressInfo.SecondLine := '';
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
