{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit frm_MarkPictureEditor;

interface

uses
  Types,
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComCtrls,
  StdCtrls,
  Spin,
  GR32,
  GR32_Image,
  GR32_Layers,
  i_PathConfig,
  i_Bitmap32Static,
  i_MarkPicture,
  i_MarkPictureConfig,
  i_LanguageManager,
  t_GeoTypes,
  u_CommonFormAndFrameParents;

type
  TfrmMarkPictureEditor = class(TFormWitghLanguageManager)
    imgIcon: TImage32;
    btnApply: TButton;
    btnCancel: TButton;
    lblMousePosition: TLabel;
    cbbAnchorType: TComboBox;
    seAnchorX: TSpinEdit;
    lblAnchorX: TLabel;
    seAnchorY: TSpinEdit;
    lblAnchorY: TLabel;
    grpAnchor: TGroupBox;
    procedure imgIconMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure imgIconMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgIconMouseLeave(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure cbbAnchorTypeChange(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure seAnchorXYChange(Sender: TObject);
  private
    FScaledBitmap: TBitmap32;
    FBitmapStatic: IBitmap32Static;
    FScale: Double;
    FIconSize: TPoint;
    FOldAnchor, FNewAnchor: TDoublePoint;
    FPicName: string;
    FMarksIconsPath: IPathConfig;
    FMarkPictureConfig: IMarkPictureConfig;
    FIgnoreNotifyEvent: Boolean;
    procedure SetUpComboBox;
    procedure SetUpSpinEdits;
    procedure SetUpBitmap;
    function PixelPosScaledToPos(const X, Y: Integer): TPoint;
  public
    procedure EditMarkPicture(const APic: IMarkPicture);
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMarksIconsPath: IPathConfig;
      const AMarkPictureConfig: IMarkPictureConfig
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  GR32_Resamplers,
  u_GeoFunc,
  u_BitmapFunc,
  u_MarkPictureAnchorFunc;

resourcestring
  rsPositionInfo = 'Size: %dx%d; Mouse at: %dx%d';

{$R *.dfm}

{ TfrmMarkPictureEditor }

constructor TfrmMarkPictureEditor.Create(
  const ALanguageManager: ILanguageManager;
  const AMarksIconsPath: IPathConfig;
  const AMarkPictureConfig: IMarkPictureConfig
);
begin
  inherited Create(ALanguageManager);

  FMarksIconsPath := AMarksIconsPath;
  FMarkPictureConfig := AMarkPictureConfig;

  FIgnoreNotifyEvent := False;

  cbbAnchorType.Items.Add('Custom');
  cbbAnchorType.Items.Add('Default');
  AddAnchorNamesToList(cbbAnchorType.Items);
end;

destructor TfrmMarkPictureEditor.Destroy;
begin
  FreeAndNil(FScaledBitmap);
  inherited Destroy;
end;

procedure TfrmMarkPictureEditor.EditMarkPicture(const APic: IMarkPicture);
begin
  Assert(APic <> nil);

  FBitmapStatic := APic.GetMarker;
  Assert(FBitmapStatic <> nil);

  // Init vars
  FIconSize := FBitmapStatic.Size;
  FScale := imgIcon.Width / Max(FIconSize.X, FIconSize.Y);

  FPicName := IncludeTrailingPathDelimiter(FMarksIconsPath.FullPath) + APic.GetName;
  FOldAnchor := APic.GetMarker.AnchorPoint;
  FNewAnchor := FOldAnchor;

  // set up controls
  SetUpBitmap;
  imgIcon.Bitmap.Assign(FScaledBitmap);

  SetUpSpinEdits;
  SetUpComboBox;

  btnApply.Enabled := False;

  // run
  ShowModal;

  // clean
  FBitmapStatic := nil;
end;

{.$DEFINE DRAW_SCALABLE_MARKER}

procedure TfrmMarkPictureEditor.SetUpBitmap;
const
  cMarkerColor = clLightBlue32;
  cMarkerSize = {$IFDEF DRAW_SCALABLE_MARKER} 4 {$ELSE} 32 {$ENDIF};
var
  VTmp: TBitmap32;
  VMarkerPos: TPoint;
  X1, X2, Y1, Y2: Integer;
  VSizeTarget: TPoint;
  VResampler: TKernelResampler;
begin
  VTmp := TBitmap32.Create;
  try
    AssignStaticToBitmap32(VTmp, FBitmapStatic);

    {$IFDEF DRAW_SCALABLE_MARKER}
    VMarkerPos := AnchorAbsoluteToPoint(FNewAnchor, FIconSize);

    Dec(VMarkerPos.X);
    Dec(VMarkerPos.Y);

    X1 := VMarkerPos.X - cMarkerSize;
    X2 := VMarkerPos.X + cMarkerSize;

    VTmp.HorzLineS(X1, VMarkerPos.Y, X2, cMarkerColor);

    Y1 := VMarkerPos.Y - cMarkerSize;
    Y2 := VMarkerPos.Y + cMarkerSize;

    VTmp.VertLineS(VMarkerPos.X, Y1, Y2, cMarkerColor);
    {$ENDIF}

    if FScaledBitmap = nil then begin
      FScaledBitmap := TBitmap32.Create;
    end;

    VSizeTarget.X := Trunc(FIconSize.X * FScale + 0.5);
    VSizeTarget.Y := Trunc(FIconSize.Y * FScale + 0.5);

    FScaledBitmap.SetSize(VSizeTarget.X, VSizeTarget.Y);
    FScaledBitmap.Clear(clWhite32);

    VResampler := TKernelResampler.Create;
    try
      VResampler.Kernel := TLanczosKernel.Create;
      GR32_Resamplers.StretchTransfer(
        FScaledBitmap,
        FScaledBitmap.BoundsRect,
        FScaledBitmap.ClipRect,
        VTmp.Bits,
        FIconSize.X,
        FIconSize.Y,
        Bounds(0, 0, FIconSize.X, FIconSize.Y),
        VResampler,
        dmBlend,
        cmBlend,
        255,
        0
      );
    finally
      VResampler.Free;
    end;

    {$IFNDEF DRAW_SCALABLE_MARKER}
    VMarkerPos := AnchorAbsoluteToPoint(FNewAnchor, FIconSize);

    Dec(VMarkerPos.X);
    Dec(VMarkerPos.Y);

    X1 := Floor(VMarkerPos.X * FScale + 0.001);
    Y1 := Floor(VMarkerPos.Y * FScale + 0.001);

    X2 := Floor((VMarkerPos.X + 1) * FScale + 0.001);
    Y2 := Floor((VMarkerPos.Y + 1) * FScale + 0.001);

    if (X2 - X1) <= 1 then begin
      Dec(X1);
      Inc(X2);
    end;

    if (Y2 - Y1) <= 1 then begin
      Dec(Y1);
      Inc(Y2);
    end;

    FScaledBitmap.FillRectS(X1 - cMarkerSize, Y1, X2 + cMarkerSize, Y2, cMarkerColor);
    FScaledBitmap.FillRectS(X1, Y1 - cMarkerSize, X2, Y2 + cMarkerSize, cMarkerColor);
    {$ENDIF}
  finally
    VTmp.Free;
  end;
end;

procedure TfrmMarkPictureEditor.SetUpComboBox;
var
  I: Integer;
  VOldAnchorRelative: TDoublePoint;
  VDefaultAnchor: TDoublePoint;
  VAnchorName: string;
  VPoint: TPoint;
begin
  FIgnoreNotifyEvent := True;

  VOldAnchorRelative := AnchorAbsoluteToRelative(FOldAnchor, FIconSize);

  VDefaultAnchor := FMarkPictureConfig.GetDefaultAnchor(FPicName);
  if IsKnownAnchor(VDefaultAnchor, VAnchorName) then begin
    cbbAnchorType.Items.Strings[1] := 'Default [' + VAnchorName + ']';
  end else begin
    VPoint := AnchorRelativeToPoint(VDefaultAnchor, FIconSize);
    cbbAnchorType.Items.Strings[1] :=
      Format('Default [X: %d; Y: %d]', [VPoint.X, VPoint.Y]);
  end;

  if DoublePointsEqual(VDefaultAnchor, VOldAnchorRelative) then begin
    I := 1;
  end else if IsKnownAnchor(VOldAnchorRelative, VAnchorName) then begin
    I := cbbAnchorType.Items.IndexOf(VAnchorName);
  end else begin
    I := 0;
  end;

  if I >= 0 then begin
    cbbAnchorType.ItemIndex := I;
  end;

  FIgnoreNotifyEvent := False;
end;

procedure TfrmMarkPictureEditor.SetUpSpinEdits;
var
  VPoint: TPoint;
begin
  FIgnoreNotifyEvent := True;

  VPoint := AnchorAbsoluteToPoint(FOldAnchor, FIconSize);

  lblAnchorX.Caption := Format('X (1..%d):', [FIconSize.X]);
  seAnchorX.MinValue := 1;
  seAnchorX.MaxValue := FIconSize.X;
  seAnchorX.Increment := 1;
  seAnchorX.Value := VPoint.X;

  lblAnchorY.Caption := Format('Y (1..%d):', [FIconSize.Y]);
  seAnchorY.MinValue := 1;
  seAnchorY.MaxValue := FIconSize.Y;
  seAnchorY.Increment := 1;
  seAnchorY.Value := VPoint.Y;

  FIgnoreNotifyEvent := False;
end;

procedure TfrmMarkPictureEditor.imgIconMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  VPos: TPoint;
begin
  FIgnoreNotifyEvent := True;

  VPos := PixelPosScaledToPos(X, Y);

  seAnchorX.Value := VPos.X;
  seAnchorY.Value := VPos.Y;

  FNewAnchor := DoublePoint(VPos);
  btnApply.Enabled := not DoublePointsEqual(FOldAnchor, FNewAnchor);

  cbbAnchorType.ItemIndex := 0;

  SetUpBitmap;
  imgIcon.Bitmap.Assign(FScaledBitmap);

  FIgnoreNotifyEvent := False;
end;

procedure TfrmMarkPictureEditor.imgIconMouseLeave(Sender: TObject);
begin
  imgIcon.Bitmap.Assign(FScaledBitmap);
  lblMousePosition.Caption := '';
end;

procedure TfrmMarkPictureEditor.imgIconMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  VPos: TPoint;
  VBitmapSize: TSize;
begin
  VBitmapSize := imgIcon.GetBitmapSize;
  if (X >= 0) and (X < VBitmapSize.cx) and (Y >= 0) and (Y < VBitmapSize.cy) then begin
    imgIcon.Bitmap.Lock;
    try
      imgIcon.Bitmap.Assign(FScaledBitmap);
      imgIcon.Bitmap.HorzLineS(0, Y, VBitmapSize.cx - 1, clLtGray);
      imgIcon.Bitmap.VertLineS(X, 0, VBitmapSize.cy - 1, clLtGray);
    finally
      imgIcon.Bitmap.UnLock;
    end;

    VPos := PixelPosScaledToPos(X, Y);

    lblMousePosition.Caption :=
      Format(
        rsPositionInfo,
        [FIconSize.X, FIconSize.Y, VPos.X, VPos.Y]
      );
  end else begin
    imgIcon.Bitmap.Assign(FScaledBitmap);
    lblMousePosition.Caption := '';
  end;
end;

function TfrmMarkPictureEditor.PixelPosScaledToPos(const X, Y: Integer): TPoint;
begin
  Result.X := Ceil((X + 1) / FScale);
  Result.Y := Ceil((Y + 1) / FScale);

  Assert(Result.X >= 1);
  Assert(Result.X <= FIconSize.X);
  Assert(Result.Y >= 1);
  Assert(Result.Y <= FIconSize.Y);
end;

procedure TfrmMarkPictureEditor.btnApplyClick(Sender: TObject);
var
  VAnchor: TDoublePoint;
begin
  VAnchor := AnchorAbsoluteToRelative(FNewAnchor, FIconSize);
  FMarkPictureConfig.SetAnchor(FPicName, VAnchor);
  Close;
end;

procedure TfrmMarkPictureEditor.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMarkPictureEditor.seAnchorXYChange(Sender: TObject);
var
  VPoint: TPoint;
begin
  if FIgnoreNotifyEvent then begin
    Exit;
  end;

  FIgnoreNotifyEvent := True;

  VPoint.X := seAnchorX.Value;
  VPoint.Y := seAnchorY.Value;

  FNewAnchor := DoublePoint(VPoint);
  btnApply.Enabled := not DoublePointsEqual(FOldAnchor, FNewAnchor);

  cbbAnchorType.ItemIndex := 0;

  SetUpBitmap;
  imgIcon.Bitmap.Assign(FScaledBitmap);

  FIgnoreNotifyEvent := False;
end;

procedure TfrmMarkPictureEditor.cbbAnchorTypeChange(Sender: TObject);
var
  I: Integer;
  VPoint: TPoint;
  VAnchor: TDoublePoint;
begin
  if FIgnoreNotifyEvent then begin
    Exit;
  end;

  FIgnoreNotifyEvent := True;

  I := cbbAnchorType.ItemIndex;

  if I = 1 then begin
    FNewAnchor := AnchorRelativeToAbsolute(
      FMarkPictureConfig.GetDefaultAnchor(FPicName),
      FIconSize
    )
  end else if I > 1 then begin
    VAnchor := GetAnchorFromName(cbbAnchorType.Items.Strings[I]);
    if not PointIsEmpty(VAnchor) then begin
      FNewAnchor := AnchorRelativeToAbsolute(VAnchor, FIconSize);
    end;
  end;

  VPoint := AnchorAbsoluteToPoint(FNewAnchor, FIconSize);

  seAnchorX.Value := VPoint.X;
  seAnchorY.Value := VPoint.Y;

  btnApply.Enabled := not DoublePointsEqual(FOldAnchor, FNewAnchor);

  SetUpBitmap;
  imgIcon.Bitmap.Assign(FScaledBitmap);

  FIgnoreNotifyEvent := False;
end;

end.
