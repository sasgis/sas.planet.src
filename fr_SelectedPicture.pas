unit fr_SelectedPicture;

interface

uses
  Classes,
  Controls,
  Forms,
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
  GR32_Resamplers,
  u_BitmapFunc;

{$R *.dfm}

{ TfrSelectedPicture }

procedure TfrSelectedPicture.CopyMarkerToBitmap(
  const ASourceBitmap: IBitmap32Static; ATarget: TCustomBitmap32);
var
  VSourceSize: TPoint;
  VScale: Double;
  VSourceRect: TRect;
  VDstRect: TRect;
  VResampler: TCustomResampler;
begin
  VSourceSize := ASourceBitmap.Size;
  if (VSourceSize.X > 0) and (VSourceSize.Y > 0) then begin
    ATarget.Clear(clWhite32);
    VScale := Min(ATarget.Width / VSourceSize.X, ATarget.Height / VSourceSize.Y);
    VSourceRect := Rect(0, 0, VSourceSize.X, VSourceSize.Y);
    VDstRect :=
      Rect(
        Trunc((ATarget.Width - VSourceSize.X * VScale) / 2),
        Trunc((ATarget.Height - VSourceSize.Y * VScale) / 2),
        Trunc((ATarget.Width + VSourceSize.X * VScale) / 2),
        Trunc((ATarget.Height + VSourceSize.Y * VScale) / 2)
      );
    VResampler := TLinearResampler.Create;
    try
      StretchTransfer(
        ATarget,
        VDstRect,
        ASourceBitmap,
        VSourceRect,
        VResampler,
        dmBlend,
        cmBlend
      );
    finally
      VResampler.Free;
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
