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
    function DrawToBitmap(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    ): Boolean;
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

function TMarkerDrawableCaptionSimple.DrawToBitmap(ABitmap: TCustomBitmap32;
  const APosition: TDoublePoint): Boolean;
begin
  Result := False;
end;

end.
