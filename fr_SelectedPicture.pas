unit fr_SelectedPicture;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  GR32,
  GR32_Image,
  GR32_Layers,
  i_MarkPicture,
  i_Bitmap32Static,
  i_LanguageManager,
  u_CommonFormAndFrameParents;

type
  TfrSelectedPicture = class(TFrame)
    imgIcon: TImage32;
    procedure imgIconMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgIconResize(Sender: TObject);
  private
    FPicture: IMarkPicture;
    FOnClick: TNotifyEvent;
    procedure SetPicture(const Value: IMarkPicture);
    procedure CopyMarkerToBitmap(
      const ASourceBitmap: IBitmap32Static;
      ATarget: TCustomBitmap32
    );
    procedure UpdatePicture;
  public
    property Picture: IMarkPicture read FPicture write SetPicture;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      AOnClick: TNotifyEvent
    ); reintroduce;
  end;

implementation

uses
  Types,
  Math,
  GR32_Blend,
  GR32_Rasterizers,
  GR32_Resamplers,
  GR32_Transforms,
  u_BitmapFunc;

{$R *.dfm}

{ TfrSelectedPicture }

procedure TfrSelectedPicture.CopyMarkerToBitmap(
  const ASourceBitmap: IBitmap32Static; ATarget: TCustomBitmap32);
var
  VTransform: TAffineTransformation;
  VSizeSource: TPoint;
  VTargetRect: TFloatRect;
  VScale: Double;
  VRasterizer: TRasterizer;
  VTransformer: TTransformer;
  VCombineInfo: TCombineInfo;
  VSampler: TCustomResampler;
  VBitmap: TCustomBitmap32;
begin
  VSizeSource := ASourceBitmap.Size;
  if (VSizeSource.X > 0) and (VSizeSource.Y > 0) then begin
    VTransform := TAffineTransformation.Create;
    try
      VTransform.SrcRect := FloatRect(0, 0, VSizeSource.X, VSizeSource.Y);
      VScale := Min(ATarget.Width / VSizeSource.X, ATarget.Height / VSizeSource.Y);
      VTransform.Translate(-VSizeSource.X / 2, -VSizeSource.Y /2);
      VTransform.Scale(VScale, VScale);
      VTransform.Translate(ATarget.Width / 2, ATarget.Height /2);
      VTargetRect := VTransform.GetTransformedBounds;

      ATarget.Clear(clWhite32);

      VRasterizer := TRegularRasterizer.Create;
      try
        VSampler := TLinearResampler.Create;
        try
          VBitmap := TCustomBitmap32.Create;
          try
            AssignStaticToBitmap32(VBitmap, ASourceBitmap);
            VSampler.Bitmap := VBitmap;
            VTransformer := TTransformer.Create(VSampler, VTransform);
            try
              VRasterizer.Sampler := VTransformer;
              VCombineInfo.SrcAlpha := 255;
              VCombineInfo.DrawMode := dmBlend;
              VCombineInfo.CombineMode := cmBlend;
              VCombineInfo.TransparentColor := 0;
              VRasterizer.Rasterize(ATarget, ATarget.BoundsRect, VCombineInfo);
            finally
              EMMS;
              VTransformer.Free;
            end;
          finally
            VBitmap.Free;
          end;
        finally
          VSampler.Free;
        end;
      finally
        VRasterizer.Free;
      end;
    finally
      VTransform.Free;
    end;
  end;
end;

constructor TfrSelectedPicture.Create(const ALanguageManager: ILanguageManager;
  AOnClick: TNotifyEvent);
begin
  inherited Create(ALanguageManager);
  FOnClick := AOnClick;
end;

procedure TfrSelectedPicture.imgIconMouseDown(Sender: TObject; Button:
    TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if Assigned(FOnClick) then begin
    FOnClick(Self);
  end;
end;

procedure TfrSelectedPicture.imgIconResize(Sender: TObject);
begin
  UpdatePicture;
end;

procedure TfrSelectedPicture.SetPicture(const Value: IMarkPicture);
begin
  FPicture := Value;
  UpdatePicture;
end;

procedure TfrSelectedPicture.UpdatePicture;
begin
  if FPicture <> nil then begin
    imgIcon.Bitmap.SetSizeFrom(imgIcon);
    CopyMarkerToBitmap(FPicture.GetMarker, imgIcon.Bitmap);
    imgIcon.Hint := FPicture.GetName;
  end else begin
    imgIcon.Bitmap.Delete;
    imgIcon.Hint := '';
  end;
end;

end.
