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
    FShowOnFormLine0: string;
    FShowOnFormLine1: string;
    FProgressOnForm: integer;
    FMessageForShow: string;

    FNumImgs: integer;
    FNumImgsSaved: integer;

    function IsCancel: Boolean;
    procedure UpdateProgressFormBar;
    procedure UpdateProgressFormStr1;
    procedure UpdateProgressFormStr2;
    procedure UpdateProgressFormClose;
    procedure SynShowMessage;

    procedure Execute; override;
  public
    constructor Create(
      AMapCalibrationList: IInterfaceList;
      AFileName: string;
      APolygon: TPointArray;
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
  UResStrings,
  Ugeofun;

{ TMapCombineThreadBase }

constructor TMapCombineThreadBase.Create(
  AMapCalibrationList: IInterfaceList;
  AFileName: string;
  APolygon: TPointArray;
  ASplitCount: TPoint;
  Azoom: byte;
  Atypemap: TMapType;
  AHtypemap: TMapType;
  AusedReColor,
  AusedMarks: boolean
);
var
  VProcessTiles: Int64;
begin
  inherited Create(false);
  Priority := tpLower;
  FreeOnTerminate := true;
  FPoly := APolygon;
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
  FProgressForm.Visible := true;

  VProcessTiles := GetDwnlNum(FMapRect.TopLeft, FMapRect.BottomRight, FPoly, true);
  GetMinMax(FMapRect.TopLeft, FMapRect.BottomRight, FPoly,false);

  FMapSize.X := FMapRect.Right - FMapRect.Left;
  FMapSize.Y := FMapRect.Bottom - FMapRect.Top;
  FMapPieceSize.X := FMapSize.X div FSplitCount.X;
  FMapPieceSize.Y := FMapSize.Y div FSplitCount.Y;
  FProgressForm.ProgressBar1.Max := FMapPieceSize.Y;

  FProgressForm.Caption := 'Ñךכוטעü: '+inttostr((FMapSize.X) div 256+1)+'x'
    +inttostr((FMapSize.Y) div 256+1) +'('+inttostr(VProcessTiles)+') '+SAS_STR_files;
end;

procedure TMapCombineThreadBase.SynShowMessage;
begin
  ShowMessage(FMessageForShow);
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

procedure TMapCombineThreadBase.Execute;
begin
  inherited;

end;

end.
