unit u_ThreadMapCombineBase;

interface

uses
  Classes,
  Types,
  t_GeoTypes,
  UMapType,
  u_ThreadRegionProcessAbstract;

type
  TThreadMapCombineBase = class(TThreadRegionProcessAbstract)
  protected
    FTypeMap: TMapType;
    FHTypeMap: TMapType;
    FZoom: byte;
    FPoly: TPointArray;
    FMapCalibrationList: IInterfaceList;
    FSplitCount: TPoint;
    FUsedReColor: boolean;
    FUsedMarks: boolean;

    FFileName: string;
    FFilePath: string;
    FFileExt: string;
    FCurrentFileName: string;
    FMapRect: TRect;
    FMapSize: TPoint;
    FMapPieceSize: TPoint;
    FCurrentPieceRect: TRect;
    FLastTile: TPoint;

    FNumImgs: integer;
    FNumImgsSaved: integer;

    procedure ProgressFormUpdateOnProgress; virtual;

    procedure saveRECT; virtual; abstract;

    procedure ProcessRegion; override;
  public
    constructor Create(
      AMapCalibrationList: IInterfaceList;
      AFileName: string;
      APolygon: TDoublePointArray;
      ASplitCount: TPoint;
      Azoom: byte;
      Atypemap: TMapType;
      AHtypemap: TMapType;
      AusedReColor,
      AusedMarks: boolean
    );
  end;

implementation

uses
  SysUtils,
  i_IMapCalibration,
  UResStrings,
  Ugeofun;

{ TMapCombineThreadBase }

constructor TThreadMapCombineBase.Create(
  AMapCalibrationList: IInterfaceList;
  AFileName: string;
  APolygon: TDoublePointArray;
  ASplitCount: TPoint;
  Azoom: byte;
  Atypemap: TMapType;
  AHtypemap: TMapType;
  AusedReColor,
  AusedMarks: boolean
);
begin
  inherited Create(APolygon);
  FZoom := Azoom - 1;
  FSplitCount := ASplitCount;
  FFilePath := ExtractFilePath(AFileName);
  FFileExt := ExtractFileExt(AFileName);
  FFileName := ChangeFileExt(ExtractFileName(AFileName), '');
  FTypeMap := Atypemap;
  FHTypeMap := AHtypemap;
  FUsedReColor := AusedReColor;
  FUsedMarks := AusedMarks;
  FMapCalibrationList := AMapCalibrationList;
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


procedure TThreadMapCombineBase.ProcessRegion;
var
  i, j, pti: integer;
  VProcessTiles: Int64;
begin
  inherited;
  FPoly := FTypeMap.GeoConvert.LonLatArray2PixelArray(FPolygLL, FZoom);

  VProcessTiles := GetDwnlNum(FMapRect.TopLeft, FMapRect.BottomRight, FPoly, true);
  GetMinMax(FMapRect.TopLeft, FMapRect.BottomRight, FPoly, false);

  FMapSize.X := FMapRect.Right - FMapRect.Left;
  FMapSize.Y := FMapRect.Bottom - FMapRect.Top;
  FMapPieceSize.X := FMapSize.X div FSplitCount.X;
  FMapPieceSize.Y := FMapSize.Y div FSplitCount.Y;

  FTilesToProcess := FMapPieceSize.Y;
  FTilesProcessed := 0;

  FNumImgs := FSplitCount.X * FSplitCount.Y;
  FNumImgsSaved := 0;

  ProgressFormUpdateCaption(
    Format(
      SAS_STR_MapCombineProgressLine0,
      [FMapSize.X div 256 + 1, FMapSize.Y div 256 + 1, VProcessTiles]
    ),
    Format(
      SAS_STR_MapCombineProgressCaption,
      [FMapSize.X, FMapSize.Y, FNumImgs]
    )
  );

  ProgressFormUpdateOnProgress;

  for i := 1 to FSplitCount.X do begin
    for j := 1 to FSplitCount.Y do begin
      FCurrentPieceRect.Left := FMapRect.Left + FMapPieceSize.X * (i - 1);
      FCurrentPieceRect.Right := FMapRect.Left + FMapPieceSize.X * i;
      FCurrentPieceRect.Top := FMapRect.Top + FMapPieceSize.Y * (j - 1);
      FCurrentPieceRect.Bottom := FMapRect.Top + FMapPieceSize.Y * j;

      if (FSplitCount.X > 1) or (FSplitCount.Y > 1) then begin
        FCurrentFileName := FFilePath + FFileName + '_' + inttostr(i) + '-' + inttostr(j) + FFileExt;
      end else begin
        FCurrentFileName := FFilePath + FFileName + FFileExt;
      end;

      for pti := 0 to FMapCalibrationList.Count - 1 do begin
        try
          (FMapCalibrationList.get(pti) as IMapCalibration).SaveCalibrationInfo(FCurrentFileName, FCurrentPieceRect.TopLeft, FCurrentPieceRect.BottomRight, FZoom, FTypeMap.GeoConvert);
        except
          //TODO: ƒобавить сюда нормальную обработку ошибок.
        end;
      end;
      saveRECT;
    end;
  end;
end;

end.
