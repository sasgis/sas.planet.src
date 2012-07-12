unit u_MarkerDrawableSimpleAbstract;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarkerDrawable,
  i_MarkerSimpleConfig;

type
  TMarkerDrawableSimpleBaseAbstract = class(TInterfacedObject)
  private
    FConfig: IMarkerSimpleConfigStatic;
  protected
    property Config: IMarkerSimpleConfigStatic read FConfig;
  public
    constructor Create(const AConfig: IMarkerSimpleConfigStatic); virtual;
  end;

  TMarkerDrawableSimpleAbstract = class(TMarkerDrawableSimpleBaseAbstract, IMarkerDrawable)
  protected
    procedure DrawToBitmap(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    ); virtual; abstract;
  end;
  TMarkerDrawableSimpleAbstractClass = class of TMarkerDrawableSimpleAbstract;

  TMarkerDrawableWithDirectionSimpleAbstract = class(TMarkerDrawableSimpleBaseAbstract, IMarkerDrawableWithDirection)
  protected
    procedure DrawToBitmapWithDirection(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint;
      const AAngle: Double
    ); virtual; abstract;
  end;
  TMarkerDrawableWithDirectionSimpleAbstractClass = class of TMarkerDrawableWithDirectionSimpleAbstract;


implementation

{ TMarkerDrawableSimpleBaseAbstract }

constructor TMarkerDrawableSimpleBaseAbstract.Create(
  const AConfig: IMarkerSimpleConfigStatic);
begin
  inherited Create;
  FConfig := AConfig;
end;

end.

