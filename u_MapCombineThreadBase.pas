unit u_MapCombineThreadBase;

interface

uses
  Classes,
  Types,
  t_GeoTypes,
  UMapType,
  unit4;

type
  TMapCombineThreadBase = class(TThread)
  protected
    FPolygLL: TExtendedPointArray;
    FTilesToProcess: Int64;
    FTilesProcessed: Int64;

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

    FProgressForm: TFprogress2;
    FShowFormCaption: string;
    FShowOnFormLine0: string;
    FShowOnFormLine1: string;
    FProgressOnForm: integer;
    FMessageForShow: string;

    FNumImgs: integer;
    FNumImgsSaved: integer;

    function IsCancel: Boolean;
    procedure UpdateProgressFormBar;
    procedure UpdateProgressFormCaption;
    procedure UpdateProgressFormStr1;
    procedure UpdateProgressFormStr2;
    procedure UpdateProgressFormClose;
    procedure SynShowMessage;

    procedure ProgressFormUpdateProgressAndLine1(AProgress: Integer; ALine1: string);
    procedure ProgressFormUpdateProgressLine0AndLine1(AProgress: Integer; ALine0, ALine1: string);
    procedure ProgressFormUpdateCaption(ALine0, ACaption: string);
    procedure ProgressFormUpdateOnProgress; virtual;

    procedure ShowMessageSync(AMessage: string);

    procedure saveRECT; virtual; abstract;

    procedure Execute; override;
  public
    constructor Create(
      AMapCalibrationList: IInterfaceList;
      AFileName: string;
      APolygon: TExtendedPointArray;
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
  Forms,
  Dialogs,
  i_IMapCalibration,
  UResStrings,
  Ugeofun;

{ TMapCombineThreadBase }

constructor TMapCombineThreadBase.Create(
  AMapCalibrationList: IInterfaceList;
  AFileName: string;
  APolygon: TExtendedPointArray;
  ASplitCount: TPoint;
  Azoom: byte;
  Atypemap: TMapType;
  AHtypemap: TMapType;
  AusedReColor,
  AusedMarks: boolean
);
begin
  inherited Create(false);
  Priority := tpLower;
  FreeOnTerminate := true;
  FPolygLL := Copy(APolygon);
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

  Application.CreateForm(TFProgress2, FProgressForm);
  FProgressForm.ProgressBar1.Progress1 := 0;
  FProgressForm.ProgressBar1.Max := 100;
  FProgressForm.Visible := true;

end;

procedure TMapCombineThreadBase.ShowMessageSync(AMessage: string);
begin
  FMessageForShow := AMessage;
  Synchronize(SynShowMessage);
end;

procedure TMapCombineThreadBase.SynShowMessage;
begin
  ShowMessage(FMessageForShow);
end;

procedure TMapCombineThreadBase.UpdateProgressFormCaption;
begin
  FProgressForm.Caption := FShowFormCaption;
end;

procedure TMapCombineThreadBase.UpdateProgressFormClose;
begin
  FProgressForm.Close;
end;

procedure TMapCombineThreadBase.UpdateProgressFormStr1;
begin
  FProgressForm.MemoInfo.Lines[0] := FShowOnFormLine0;
end;

procedure TMapCombineThreadBase.UpdateProgressFormStr2;
begin
  FProgressForm.MemoInfo.Lines[1] := FShowOnFormLine1;
end;

procedure TMapCombineThreadBase.UpdateProgressFormBar;
begin
  FProgressForm.ProgressBar1.Progress1 := FProgressOnForm;
end;

function TMapCombineThreadBase.IsCancel: Boolean;
begin
  result := not(FProgressForm.Visible);
end;

procedure TMapCombineThreadBase.ProgressFormUpdateCaption(ALine0,
  ACaption: string);
begin
  FShowOnFormLine0 := ALine0;
  Synchronize(UpdateProgressFormStr1);
  FShowFormCaption := ACaption;
  Synchronize(UpdateProgressFormCaption);
end;

procedure TMapCombineThreadBase.ProgressFormUpdateOnProgress;
var
  VProcessed: Integer;
begin
  VProcessed := round((FTilesProcessed / FTilesToProcess) * 100);
  ProgressFormUpdateProgressAndLine1(
    VProcessed,
    SAS_STR_Processed+': '+inttostr(VProcessed)+'%'
  );
end;

procedure TMapCombineThreadBase.ProgressFormUpdateProgressAndLine1(
  AProgress: Integer; ALine1: string);
begin
  FProgressOnForm := AProgress;
  Synchronize(UpdateProgressFormBar);
  FShowOnFormLine1 := ALine1;
  Synchronize(UpdateProgressFormStr2);
end;

procedure TMapCombineThreadBase.ProgressFormUpdateProgressLine0AndLine1(
  AProgress: Integer; ALine0, ALine1: string);
begin
  FProgressOnForm := AProgress;
  Synchronize(UpdateProgressFormBar);
  FShowOnFormLine0 := ALine0;
  Synchronize(UpdateProgressFormStr1);
  FShowOnFormLine1 := ALine1;
  Synchronize(UpdateProgressFormStr2);
end;

procedure TMapCombineThreadBase.Execute;
var
  i, j, pti: integer;
  VProcessTiles: Int64;
begin
  inherited;
  FPoly := FTypeMap.GeoConvert.LonLatArray2PixelArray(FPolygLL, FZoom);

  VProcessTiles := GetDwnlNum(FMapRect.TopLeft, FMapRect.BottomRight, FPoly, true);
  GetMinMax(FMapRect.TopLeft, FMapRect.BottomRight, FPoly,false);

  FMapSize.X := FMapRect.Right - FMapRect.Left;
  FMapSize.Y := FMapRect.Bottom - FMapRect.Top;
  FMapPieceSize.X := FMapSize.X div FSplitCount.X;
  FMapPieceSize.Y := FMapSize.Y div FSplitCount.Y;

  FTilesToProcess := FMapPieceSize.Y;
  FTilesProcessed := 0;

  FNumImgs:=FSplitCount.X*FSplitCount.Y;
  FNumImgsSaved:=0;

  ProgressFormUpdateCaption(
    'Склеить: '+inttostr((FMapSize.X) div 256+1)+'x'
    +inttostr((FMapSize.Y) div 256+1) +'('+inttostr(VProcessTiles)+') '
    +SAS_STR_files,
    SAS_STR_Resolution+': '+inttostr(FMapSize.X)+'x'+inttostr(FMapSize.Y)+' '
    +SAS_STR_DivideInto+' '+inttostr(FNumImgs)+' '+SAS_STR_files
  );

  ProgressFormUpdateOnProgress;
  FCurrentFileName := FFilePath + FFileName + FFileExt;

  for i:=1 to FSplitCount.X do begin
    for j:=1 to FSplitCount.Y do begin
      FCurrentPieceRect.Left := FMapRect.Left + FMapPieceSize.X * (i-1);
      FCurrentPieceRect.Right := FMapRect.Left + FMapPieceSize.X * i;
      FCurrentPieceRect.Top := FMapRect.Top + FMapPieceSize.Y * (j-1);
      FCurrentPieceRect.Bottom := FMapRect.Top + FMapPieceSize.Y * j;

      if (FSplitCount.X > 1) or (FSplitCount.Y > 1) then begin
        FCurrentFileName := FFilePath + FFileName + '_'+inttostr(i)+'-'+inttostr(j) + FFileExt;
      end;

      for pti := 0 to FMapCalibrationList.Count - 1 do begin
        try
          (FMapCalibrationList.get(pti) as IMapCalibration).SaveCalibrationInfo(FCurrentFileName, FCurrentPieceRect.TopLeft, FCurrentPieceRect.BottomRight, FZoom, FTypeMap.GeoConvert);
        except
          //TODO: Добавить сюда нормальную обработку ошибок.
        end;
      end;
      try
          saveRECT;
      except
        On E:Exception do begin
          ShowMessageSync(E.Message);
          exit;
        end;
      end;
    end;
  end;
  Synchronize(UpdateProgressFormClose);
end;

end.
