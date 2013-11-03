unit u_BitmapMarkerProviderByCaptionForMarks;

interface

uses
  GR32,
  i_BitmapMarker,
  i_BitmapMarkerProviderByCaption;

type
  TBitmapMarkerProviderByCaptionForMarks = class(TInterfacedObject, IBitmapMarkerProviderByCaption)
  private
    FFontSize: Integer;
    FTextColor: TColor32;
    FTextBgColor: TColor32;
    FSolidBgDraw: Boolean;

    FBitmapWithText: TBitmap32;
  private
    function GetMarker(const ACaption: string): IBitmapMarker;
  public
    constructor Create(
      AFontSize: Integer;
      ATextColor: TColor32;
      ATextBgColor: TColor32;
      ASolidBgDraw: Boolean
    );
  end;


implementation

uses
  GR32_Resamplers;

{ TBitmapMarkerProviderByCaptionForMarks }

constructor TBitmapMarkerProviderByCaptionForMarks.Create(AFontSize: Integer;
  ATextColor, ATextBgColor: TColor32; ASolidBgDraw: Boolean);
begin
  inherited Create;
  FFontSize := AFontSize;
  FTextColor := ATextColor;
  FTextBgColor := ATextBgColor;
  FSolidBgDraw := ASolidBgDraw;

  FBitmapWithText := TBitmap32.Create;
  FBitmapWithText.Font.Name := 'Tahoma';
  FBitmapWithText.Font.Style := [];
  FBitmapWithText.DrawMode := dmBlend;
  FBitmapWithText.CombineMode := cmMerge;
  FBitmapWithText.Font.Size := CMaxFontSize;
  FBitmapWithText.Resampler := TLinearResampler.Create;
end;

function TBitmapMarkerProviderByCaptionForMarks.GetMarker(
  const ACaption: string): IBitmapMarker;
begin

end;

end.
