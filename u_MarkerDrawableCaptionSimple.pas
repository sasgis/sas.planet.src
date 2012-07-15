unit u_MarkerDrawableCaptionSimple;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarkerDrawable;

type
  TMarkerDrawableCaptionSimple = class(TInterfacedObject, IMarkerDrawable)
  private
    FFontSize: Integer;
    FCapion: string;
    FTextColor: TColor32;
    FTextBgColor: TColor32;
    FSolidBgDraw: Boolean;
  private
    procedure DrawToBitmap(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    );
  public
    constructor Create(
      ACapion: string;
      AFontSize: Integer;
      ATextColor: TColor32;
      ATextBgColor: TColor32;
      ASolidBgDraw: Boolean
    );
  end;

implementation

{ TMarkerDrawableCaptionSimple }

constructor TMarkerDrawableCaptionSimple.Create(
  ACapion: string;
  AFontSize: Integer;
  ATextColor, ATextBgColor: TColor32;
  ASolidBgDraw: Boolean
);
begin
  inherited Create;
  FFontSize := AFontSize;
  FCapion := ACapion;
  FTextColor := ATextColor;
  FTextBgColor := ATextBgColor;
  FSolidBgDraw := ASolidBgDraw;
end;

procedure TMarkerDrawableCaptionSimple.DrawToBitmap(ABitmap: TCustomBitmap32;
  const APosition: TDoublePoint);
begin

end;

end.
