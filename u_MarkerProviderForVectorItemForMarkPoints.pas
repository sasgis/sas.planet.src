unit u_MarkerProviderForVectorItemForMarkPoints;

interface

uses
  i_MarkerDrawable,
  i_VectorDataItemSimple,
  i_MarksDrawConfig,
  i_MarkerProviderForVectorItem;

type
  TMarkerProviderForVectorItemForMarkPoints = class(TInterfacedObject, IMarkerProviderForVectorItem)
  private
    FConfig: IMarksDrawConfigStatic;
  private
    function GetMarker(const AItem: IVectorDataItemSimple): IMarkerDrawable;
  public
    constructor Create(const AConfig: IMarksDrawConfigStatic);
  end;

implementation

uses
  Types,
  SysUtils,
  t_GeoTypes,
  i_MarksSimple,
  i_BitmapMarker;

{ TMarkerProviderForVectorItemForMarkPoints }

constructor TMarkerProviderForVectorItemForMarkPoints.Create(
  const AConfig: IMarksDrawConfigStatic);
begin
  inherited Create;
  FConfig := AConfig;
end;

function TMarkerProviderForVectorItemForMarkPoints.GetMarker(
  const AItem: IVectorDataItemSimple): IMarkerDrawable;
var
  VLocalPoint: TDoublePoint;
  VLonLat: TDoublePoint;
  VDstRect: TRect;
  VSrcRect: TRect;
  VTextSize: TSize;
  VMarkSize: Integer;
  VFontSize: Integer;
  VMarker: IBitmapMarker;
  VTargetPoint: TPoint;
  VMarkPoint: IMarkPoint;
begin
  Result := nil;
  if not Supports(AItem, IMarkPoint, VMarkPoint) then begin
    Exit;
  end;

  VMarkSize := VMarkPoint.MarkerSize;
  VFontSize := VMarkPoint.FontSize;
  if (VMarkPoint.Pic <> nil) then begin
    VMarker := VMarkPoint.Pic.GetMarkerBySize(VMarkSize);
  end;
end;

end.
