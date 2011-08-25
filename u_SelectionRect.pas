unit u_SelectionRect;

interface

uses
  Classes,
  Types,
  t_GeoTypes,
  i_SelectionRect,
  i_ViewPortState,
  i_MapLayerGridsConfig,
  u_ConfigDataElementBase;

type
  TSelectionRect = class(TConfigDataElementBaseEmptySaveLoad, ISelectionRect)
  private
    FViewPortState: IViewPortState;
    FTileGridConfig: ITileGridConfig;
    FGenShtabGridConfig: IGenShtabGridConfig;

    FIsEmpty: Boolean;
    FFirstPoint: TDoublePoint;
    FSecondPoint: TDoublePoint;
    FShift: TShiftState;
    FResultRect: TDoubleRect;
    function PrepareSelectionRect(
      const APoint1, APoint2: TDoublePoint;
      Shift: TShiftState
    ): TDoubleRect;
  protected
    function IsEmpty: Boolean;
    procedure SetNextPoint(ALonLat: TDoublePoint; Shift: TShiftState);
    procedure Reset;
    function GetRect: TDoubleRect;
  public
    constructor Create(
      AViewPortState: IViewPortState;
      ATileGridConfig: ITileGridConfig;
      AGenShtabGridConfig: IGenShtabGridConfig
    );
  end;

implementation

uses
  i_LocalCoordConverter,
  i_CoordConverter,
  u_GeoFun;

{ TSelectionRect }

constructor TSelectionRect.Create(
  AViewPortState: IViewPortState;
  ATileGridConfig: ITileGridConfig;
  AGenShtabGridConfig: IGenShtabGridConfig
);
begin
  inherited Create;
  FTileGridConfig := ATileGridConfig;
  FGenShtabGridConfig := AGenShtabGridConfig;
  FViewPortState := AViewPortState;
  FIsEmpty := True;
end;

function TSelectionRect.GetRect: TDoubleRect;
begin
  LockRead;
  try
    Result := FResultRect;
  finally
    UnlockRead;
  end;
end;

function TSelectionRect.IsEmpty: Boolean;
begin
  LockRead;
  try
    Result := FIsEmpty;
  finally
    UnlockRead;
  end;
end;

function TSelectionRect.PrepareSelectionRect(
  const APoint1, APoint2: TDoublePoint;
  Shift: TShiftState): TDoubleRect;
var
  VConverter: ICoordConverter;
  VTemp: Double;
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := FViewPortState.GetVisualCoordConverter;
  VConverter := VLocalConverter.GetGeoConverter;

  Result.TopLeft := APoint1;
  Result.BottomRight := APoint2;

  VConverter.CheckLonLatRect(Result);
  if Result.Left > Result.Right then begin
    VTemp := Result.Left;
    Result.Left := Result.Right;
    Result.Right := VTemp;
  end;
  if Result.Top < Result.Bottom then begin
    VTemp := Result.Top;
    Result.Top := Result.Bottom;
    Result.Bottom := VTemp;
  end;
  if (ssCtrl in Shift) then begin
    Result := FTileGridConfig.GetRectStickToGrid(VLocalConverter, Result);
  end;
  if (ssShift in Shift) then begin
    Result := FGenShtabGridConfig.GetRectStickToGrid(VLocalConverter, Result);
  end;
end;

procedure TSelectionRect.Reset;
begin
  LockWrite;
  try
    if not FIsEmpty then begin
      FFirstPoint.X := 0;
      FFirstPoint.Y := 0;
      FSecondPoint.X := 0;
      FSecondPoint.Y := 0;
      FResultRect.TopLeft := FFirstPoint;
      FResultRect.BottomRight := FSecondPoint;
      FIsEmpty := True;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSelectionRect.SetNextPoint(ALonLat: TDoublePoint;
  Shift: TShiftState);
var
  VNewRect: TDoubleRect;
begin
  LockWrite;
  try
    if FIsEmpty then begin
      FIsEmpty := False;
      FFirstPoint := ALonLat;
      FSecondPoint := ALonLat;
      FShift := Shift;
      FResultRect := PrepareSelectionRect(FFirstPoint, FSecondPoint, FShift);
      SetChanged;
    end else begin
      FSecondPoint := ALonLat;
      FShift := Shift;
      VNewRect := PrepareSelectionRect(FFirstPoint, FSecondPoint, FShift);
      if not DoubleRectsEqual(FResultRect, VNewRect) then begin
        FResultRect := VNewRect;
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
