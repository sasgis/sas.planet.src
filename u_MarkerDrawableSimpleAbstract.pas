unit u_MarkerDrawableSimpleAbstract;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarkerDrawable,
  i_MarkerSimpleConfig,
  u_BaseInterfacedObject;

type
  TMarkerDrawableSimpleBaseAbstract = class(TBaseInterfacedObject)
  private
    FConfig: IMarkerSimpleConfigStatic;
  protected
    property Config: IMarkerSimpleConfigStatic read FConfig;
  public
    constructor Create(const AConfig: IMarkerSimpleConfigStatic); virtual;
  end;

  TMarkerDrawableSimpleAbstract = class(TMarkerDrawableSimpleBaseAbstract, IMarkerDrawable)
  protected
    function GetBoundsForPosition(const APosition: TDoublePoint): TRect; virtual; abstract;
    function DrawToBitmap(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    ): Boolean; virtual; abstract;
  end;
  TMarkerDrawableSimpleAbstractClass = class of TMarkerDrawableSimpleAbstract;

  TMarkerDrawableWithDirectionSimpleAbstract = class(TMarkerDrawableSimpleBaseAbstract, IMarkerDrawableWithDirection)
  protected
    function DrawToBitmapWithDirection(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint;
      const AAngle: Double
    ): Boolean; virtual; abstract;
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

