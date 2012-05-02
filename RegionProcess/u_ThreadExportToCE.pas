unit u_ThreadExportToCE;

interface

uses
  SysUtils,
  Classes,
  SAS4WinCE,
  t_GeoTypes,
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
    FMaxSize : integer;  // Максимальный размер файла
    FComent  : string;  // Коментарий
    FRecoverInfo : boolean;  // пишем копирайт ?
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      AProgressInfo: IRegionProcessProgressInfo;
      ACoordConverterFactory: ICoordConverterFactory;
      AProjectionFactory: IProjectionInfoFactory;
      AVectorItmesFactory: IVectorItmesFactory;
      ATargetFile: string;
      APolygon: ILonLatPolygon;
      Azoomarr: array of boolean;
      AMapType: TMapType;

      AMaxSize : integer;
      AComent  : string;
      ARecoverInfo : boolean

    );
  end;

implementation

uses
  Types,
  GR32,
  c_CoordConverter,
  i_CoordConverter,
  i_Bitmap32Static,
  i_TileIterator,
  i_TileInfoBasic,
  i_BinaryData,
  i_VectorItemProjected,
  i_BitmapTileSaveLoad,
  u_BitmapTileVampyreSaver,
  u_TileIteratorByPolygon,
  u_BinaryDataByMemStream,
  u_TileStorageAbstract,
  u_StreamReadOnlyByBinaryData;


constructor TThreadExportToCE.Create(
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  AProgressInfo: IRegionProcessProgressInfo;
  ACoordConverterFactory: ICoordConverterFactory;
  AProjectionFactory: IProjectionInfoFactory;
  AVectorItmesFactory: IVectorItmesFactory;
  ATargetFile: string;
  APolygon: ILonLatPolygon;
  Azoomarr: array of boolean;
  AMapType: TMapType;

  AMaxSize : integer;
  AComent  : string;
  ARecoverInfo : boolean
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
  FComent  := AComent;
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
  VSaver: IBitmapTileSaver;
  VGeoConvert: ICoordConverter;
  VSAS4WinCE:  TSAS4WinCE;
  VProjectedPolygon: IProjectedPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  TileStream : TMemoryStream;
  VData: IBinaryData;
  VTileInfo: ITileInfoBasic;
  VStream: TMemoryStream;

begin
  inherited;
  VTilesToProcess := 0;
  VSaver := TVampyreBasicBitmapTileSaverJPG.Create(100);
  VGeoConvert := FCoordConverterFactory.GetCoordConverterByCode(CGELonLatProjectionEPSG, CTileSplitQuadrate256x256);
  SetLength(VTileIterators, Length(FZooms));
  for i := 0 to Length(FZooms) - 1 do begin
    VZoom := FZooms[i];
    VProjectedPolygon :=
      FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
        FProjectionFactory.GetByConverterAndZoom(
          VGeoConvert,
          VZoom
        ),
        PolygLL
      );
    VTileIterators[i] := TTileIteratorByPolygon.Create(VProjectedPolygon);
    VTilesToProcess := VTilesToProcess + VTileIterators[i].TilesTotal;
  end;


//Начинает процесс экспорта тайлов в файл fname (без расширения!); maxsize - максимально допустимый размер файлов данных (если <0, то взять значение по умолчанию); cmt - однократно добавляемый в конец файлов комментарий; info - записывать ли в файлы данных дополнительную информацию об тайле (12-15+ байтов) и копирайт в файлы данных и файл индекса. Копирайт является также сигнатурой наличия дополнительной инфы в файлах данных!
  VSAS4WinCE := TSAS4WinCE.Create(FTargetFile, FMaxSize, FComent, FRecoverInfo);
  try
   TileStream := TMemoryStream.Create;
    try
      ProgressInfo.Caption := SAS_STR_ExportTiles;
      ProgressInfo.FirstLine := SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files;
      VTileStorage := FMapType.TileStorage;

      try

        VTilesProcessed := 0;
        ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
        for i := 0 to Length(FZooms) - 1 do begin
          VZoom := FZooms[i];
          VExt := FMapType.StorageConfig.TileFileExt;
          VTileIterator := VTileIterators[i];
          while VTileIterator.Next(VTile) do begin
            if CancelNotifier.IsOperationCanceled(OperationID) then begin
              exit;
            end;

          VData := VTileStorage.LoadTile(VTile, VZoom, nil, VTileInfo);
          if VData <> nil then begin
            VStream := TStreamReadOnlyByBinaryData.Create(VData);
//            TileStream.clear;
//            TileStream.LoadFromStream(VStream);

            try
             VSAS4WinCE.Add(
                VZoom+1,
                VTile.X,
                VTile.Y,
                VStream,
                VExt
             );
            finally
//              VStream.Free;
            end;
          end;

            inc(VTilesProcessed);
            if VTilesProcessed mod 100 = 0 then begin
              ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
            end;
          end;
        end;
      finally
        TileStream.Free;
      end;

    finally
      for i := 0 to Length(FZooms) - 1 do begin
        VTileIterators[i] := nil;
      end;
      VTileIterators := nil;
    end;
  VSAS4WinCE.SaveINX(FTargetFile,true);
  VStream.Free;

  finally
   VSAS4WinCE.Free;
  end;

end;

end.
