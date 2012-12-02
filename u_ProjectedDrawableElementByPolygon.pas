unit u_ProjectedDrawableElementByPolygon;

interface

uses
  GR32,
  GR32_Polygons,
  i_ProjectionInfo,
  i_LocalCoordConverter,
  i_VectorItemProjected,
  i_ProjectedDrawableElement,
  u_BaseInterfacedObject;

type
  TProjectedDrawableElementByPolygonSimpleEdge = class(TBaseInterfacedObject, IProjectedDrawableElement)
  private
    FSource: IProjectedPolygon;
    FColor: TColor32;
    FAntialiasMode: TAntialiasMode;
  private
    function GetProjectionInfo: IProjectionInfo;
    procedure Draw(
      ABitmap: TCustomBitmap32;
      const ALocalConverter: ILocalCoordConverter
    );
  public
    constructor Create(
      const ASource: IProjectedPolygon;
      const AAntialiasMode: TAntialiasMode;
      const AColor: TColor32
    );
  end;

implementation

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  u_GeoFun;

{ TProjectedDrawableElementByPolygonSimpleEdge }

constructor TProjectedDrawableElementByPolygonSimpleEdge.Create(
  const ASource: IProjectedPolygon;
  const AAntialiasMode: TAntialiasMode;
  const AColor: TColor32
);
begin
  Assert(ASource <> nil);
  inherited Create;
  FSource := ASource;
  FAntialiasMode := AAntialiasMode;
  FColor := AColor;
end;

procedure TProjectedDrawableElementByPolygonSimpleEdge.Draw(
  ABitmap: TCustomBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VDrawRect: TDoubleRect;
  VPolygon: TPolygon32;
  i: Integer;
  VLine: IProjectedPolygonLine;
  VPathFixedPoints: TArrayOfFixedPoint;
  VIndex: Integer;
  VEnum: IEnumProjectedPoint;
  VPoint: TDoublePoint;
  VLocalPoint: TDoublePoint;
  VIntersectRect: TDoubleRect;
begin
  if FSource.Count > 0 then begin
    VDrawRect := ALocalConverter.LocalRect2MapRectFloat(ABitmap.ClipRect);
    if IntersecProjectedRect(VIntersectRect, VDrawRect, FSource.Bounds) then begin
      if DoubleRectsEqual(VIntersectRect, FSource.Bounds) or FSource.IsRectIntersectBorder(VDrawRect) then begin
        VPolygon := TPolygon32.Create;
        try
          VPolygon.Closed := True;
          VPolygon.Antialiased := FAntialiasMode <> amNone;
          VPolygon.AntialiasMode := FAntialiasMode;

          for i := 0 to FSource.Count - 1 do begin
            VLine := FSource.Item[i];
            SetLength(VPathFixedPoints, VLine.Count + 1);
            VIndex := 0;
            VEnum := VLine.GetEnum;
            while VEnum.Next(VPoint) do begin
              VLocalPoint := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPoint);
              VPathFixedPoints[VIndex] := FixedPoint(VLocalPoint.X, VLocalPoint.Y);
              Inc(VIndex);
            end;
            VPolygon.AddPoints(VPathFixedPoints[0], VIndex);
            VPolygon.NewLine;
          end;
          VPolygon.DrawEdge(ABitmap, FColor);
          VPathFixedPoints := nil;
        finally
          VPolygon.Free;
        end;
      end;
    end;
  end;
end;

function TProjectedDrawableElementByPolygonSimpleEdge.GetProjectionInfo: IProjectionInfo;
begin
  Result := FSource.Projection;
end;

end.
