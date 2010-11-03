unit u_LastSelectionInfo;

interface

uses
  Graphics,
  GR32,
  i_JclNotify,
  t_GeoTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider;

type
  TLastSelectionInfo = class
  private
    FColor: TColor;
    FAlfa: Byte;
    FColor32: TColor32;
    FChangeNotifier: IJclNotifier;
    // Полигон последнего выделения при операциях с областью.
    FPolygon: TDoublePointArray;
    // Масштаб, на котором было последнее выделение
    FZoom: Byte;
  public
    constructor Create();
    destructor Destroy; override;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); virtual;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); virtual;
    procedure SetColor(AColor: TColor; AAlfa: Byte);
    procedure SetPolygon(APolygon: TDoublePointArray; AZoom: Byte);

    property Zoom: Byte read FZoom;
    property Polygon: TDoublePointArray read FPolygon;
    property Color: TColor read FColor;
    property Alfa: Byte read FAlfa;
    property Color32: TColor32 read FColor32;
    property ChangeNotifier: IJclNotifier read FChangeNotifier;
  end;
implementation

uses
  SysUtils,
  u_JclNotify;

{ TLastSelectionInfo }

constructor TLastSelectionInfo.Create;
begin
  FChangeNotifier := TJclBaseNotifier.Create;
  FPolygon := nil;
  FZoom := 0;
  FColor := clBlack;
  FAlfa := 210;
  FColor32 := SetAlpha(FColor, FAlfa);
end;

destructor TLastSelectionInfo.Destroy;
begin
  FChangeNotifier := nil;
  inherited;
end;

procedure TLastSelectionInfo.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  i: Integer;
  VPoint: TDoublePoint;
  VValidPoint: Boolean;
begin
  if AConfigProvider <> nil then begin
    i:=1;
    repeat
      VPoint.X := AConfigProvider.ReadFloat('PointX_'+inttostr(i), 1000000);
      VPoint.Y := AConfigProvider.ReadFloat('PointY_'+inttostr(i), 1000000);
      VValidPoint := (Abs(VPoint.X) < 360) and (Abs(VPoint.Y) < 360);
      if VValidPoint then begin
        SetLength(FPolygon, i);
        FPolygon[i - 1] := VPoint;
        inc(i);
      end;
    until not VValidPoint;
    if length(FPolygon)>0 then begin
      FZoom := AConfigProvider.Readinteger('Zoom', FZoom);
    end;

    FColor := AConfigProvider.ReadInteger('Color', clBlack);
    FAlfa := AConfigProvider.ReadInteger('Alpha', 210);
    FColor32 := SetAlpha(FColor, FAlfa);
    FChangeNotifier.Notify(nil);
  end;
end;

procedure TLastSelectionInfo.SaveConfig(
  AConfigProvider: IConfigDataWriteProvider);
var
  i: Integer;
begin
  AConfigProvider.DeleteValues;
  AConfigProvider.WriteInteger('Color', FColor);
  AConfigProvider.WriteInteger('Alfa', FAlfa);
  if Length(FPolygon)>0 then begin
    AConfigProvider.WriteInteger('Zoom', FZoom);
    for i := 0 to length(FPolygon) - 1 do begin
      AConfigProvider.WriteFloat('PointX_'+inttostr(i+1), FPolygon[i].x);
      AConfigProvider.WriteFloat('PointY_'+inttostr(i+1), FPolygon[i].y);
    end;
  end;
end;

procedure TLastSelectionInfo.SetColor(AColor: TColor; AAlfa: Byte);
begin
  FColor := AColor;
  FAlfa := AAlfa;
  FColor32 := SetAlpha(FColor, FAlfa);
  FChangeNotifier.Notify(nil);
end;

procedure TLastSelectionInfo.SetPolygon(APolygon: TDoublePointArray;
  AZoom: Byte);
begin
  FPolygon := copy(APolygon);
  FZoom := AZoom;
  FChangeNotifier.Notify(nil);
end;

end.
