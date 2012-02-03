unit u_ThreadMapCombineBase;

interface

uses
  Classes,
  Types,
  GR32,
  i_OperationNotifier,
  i_BitmapLayerProvider,
  i_VectorItemLonLat,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  u_ThreadRegionProcessAbstract;

type
  TThreadMapCombineBase = class(TThreadRegionProcessAbstract)
  private
    FTargetConverter: ILocalCoordConverter;
    FImageProvider: IBitmapLayerProvider;
    FConverterFactory: ILocalCoordConverterFactorySimpe;
    FMapCalibrationList: IInterfaceList;
    FSplitCount: TPoint;
    FFileName: string;
    FFilePath: string;
    FFileExt: string;
  protected
    procedure ProgressFormUpdateOnProgress; virtual;
    procedure SaveRect(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      AFileName: string;
      AImageProvider: IBitmapLayerProvider;
      ALocalConverter: ILocalCoordConverter;
      AConverterFactory: ILocalCoordConverterFactorySimpe
    ); virtual; abstract;

    procedure ProcessRegion; override;
  public
    constructor Create(
      APolygon: ILonLatPolygon;
      ATargetConverter: ILocalCoordConverter;
      AImageProvider: IBitmapLayerProvider;
      ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      AMapCalibrationList: IInterfaceList;
      AFileName: string;
      ASplitCount: TPoint
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_MapCalibration,
  u_ResStrings,
  u_GeoFun;

{ TMapCombineThreadBase }

constructor TThreadMapCombineBase.Create(
  APolygon: ILonLatPolygon;
  ATargetConverter: ILocalCoordConverter;
  AImageProvider: IBitmapLayerProvider;
  ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  AMapCalibrationList: IInterfaceList;
  AFileName: string;
  ASplitCount: TPoint
);
begin
  inherited Create(APolygon);
  FTargetConverter := ATargetConverter;
  FImageProvider := AImageProvider;
  FSplitCount := ASplitCount;
  FFilePath := ExtractFilePath(AFileName);
  FFileExt := ExtractFileExt(AFileName);
  FFileName := ChangeFileExt(ExtractFileName(AFileName), '');
  FMapCalibrationList := AMapCalibrationList;
  FConverterFactory := ALocalConverterFactory;

  FTilesToProcess := 1;
  FTilesProcessed := 0;
end;

procedure TThreadMapCombineBase.ProgressFormUpdateOnProgress;
var
  VProcessed: Integer;
begin
  VProcessed := round((FTilesProcessed / FTilesToProcess) * 100);
  ProgressFormUpdateProgressAndLine1(
    VProcessed,
    SAS_STR_Processed + ': ' + inttostr(VProcessed) + '%'
  );
end;


destructor TThreadMapCombineBase.Destroy;
begin
  inherited;
end;

procedure TThreadMapCombineBase.ProcessRegion;
var
  i, j, pti: integer;
  VProcessTiles: Int64;
  VTileRect: TRect;
  VCurrentPieceConverter: ILocalCoordConverter;
  VMapRect: TRect;
  VMapSize: TPoint;
  VCurrentPieceRect: TRect;
  VMapPieceSize: TPoint;
  VSizeInTile: TPoint;
  VCurrentFileName: string;
begin
  inherited;
  VMapSize := FTargetConverter.GetLocalRectSize;
  VMapRect := FTargetConverter.GetRectInMapPixel;
  VTileRect :=
    FTargetConverter.GeoConverter.PixelRect2TileRect(
      VMapRect,
      FTargetConverter.Zoom
    );
  VSizeInTile.X := VTileRect.Right - VTileRect.Left;
  VSizeInTile.Y := VTileRect.Bottom - VTileRect.Top;
  VProcessTiles := VSizeInTile.X;
  VProcessTiles := VProcessTiles * VSizeInTile.Y;

  ProgressFormUpdateCaption(
    Format(
      SAS_STR_MapCombineProgressLine0,
      [VSizeInTile.X, VSizeInTile.Y, VProcessTiles]
    ),
    Format(
      SAS_STR_MapCombineProgressCaption,
      [VMapSize.X, VMapSize.Y, FSplitCount.X * FSplitCount.Y]
    )
  );
  ProgressFormUpdateOnProgress;
  VMapPieceSize.X := VMapSize.X div FSplitCount.X;
  VMapPieceSize.Y := VMapSize.Y div FSplitCount.Y;

  for i := 1 to FSplitCount.X do begin
    for j := 1 to FSplitCount.Y do begin
      VCurrentPieceRect.Left := VMapRect.Left + VMapPieceSize.X * (i - 1);
      VCurrentPieceRect.Right := VMapRect.Left + VMapPieceSize.X * i;
      VCurrentPieceRect.Top := VMapRect.Top + VMapPieceSize.Y * (j - 1);
      VCurrentPieceRect.Bottom := VMapRect.Top + VMapPieceSize.Y * j;

      VCurrentPieceConverter :=
        FConverterFactory.CreateConverterNoScale(
          Rect(0, 0, VMapPieceSize.X, VMapPieceSize.Y),
          FTargetConverter.Zoom,
          FTargetConverter.GeoConverter,
          DoublePoint(VCurrentPieceRect.TopLeft)
        );
      if (FSplitCount.X > 1) or (FSplitCount.Y > 1) then begin
        VCurrentFileName := FFilePath + FFileName + '_' + inttostr(i) + '-' + inttostr(j) + FFileExt;
      end else begin
        VCurrentFileName := FFilePath + FFileName + FFileExt;
      end;

      for pti := 0 to FMapCalibrationList.Count - 1 do begin
        try
          (FMapCalibrationList.get(pti) as IMapCalibration).SaveCalibrationInfo(
            VCurrentFileName,
            VCurrentPieceRect.TopLeft,
            VCurrentPieceRect.BottomRight,
            FTargetConverter.Zoom,
            FTargetConverter.GeoConverter
          );
        except
          //TODO: ƒобавить сюда нормальную обработку ошибок.
        end;
      end;
      try
        SaveRect(
          OperationID,
          CancelNotifier,
          VCurrentFileName,
          FImageProvider,
          VCurrentPieceConverter,
          FConverterFactory
        );
      except
        on E: Exception do begin
          if (FSplitCount.X > 1) or (FSplitCount.Y > 1) then begin
            raise Exception.CreateFmt(
              '%0:s'#13#10'Piece %1:dx%2:d',
              [E.message, i, j]
            );
          end else begin
            raise;
          end;
        end;
      end;
    end;
  end;
end;

end.
