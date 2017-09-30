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

unit frm_MarkPictureConfig;

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
  StdCtrls,
  ExtCtrls,
  i_PathConfig,
  i_LanguageManager,
  i_MarkPicture,
  i_MarkPictureConfig,
  u_CommonFormAndFrameParents,
  frm_MarkPictureEditor;

type
  TfrmMarkPictureConfig = class(TFormWitghLanguageManager)
    btnClose: TButton;
    pnlCaptions: TPanel;
    lstItems: TListBox;
    lblIcon: TLabel;
    lblName: TLabel;
    lblAnchor: TLabel;
    btnEdit: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure lstItemsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnEditClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lstItemsDblClick(Sender: TObject);
    procedure lstItemsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FMarksIconsPath: IPathConfig;
    FMarkPictureList: IMarkPictureList;
    FMarkPictureConfig: IMarkPictureConfig;
    FfrmMarkPictureEditor: TfrmMarkPictureEditor;
  private
    procedure LoadList(AList: TStrings);
    procedure DoEditMarkPicture;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMarksIconsPath: IPathConfig;
      const AMarkPictureList: IMarkPictureList;
      const AMarkPictureConfig: IMarkPictureConfig
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  GR32,
  GR32_Resamplers,
  t_GeoTypes,
  i_Bitmap32Static,
  u_BitmapFunc,
  u_GeoFunc,
  u_MarkPictureAnchorFunc;

{$R *.dfm}

{ TfrmMarkPictureConfig }

constructor TfrmMarkPictureConfig.Create(
  const ALanguageManager: ILanguageManager;
  const AMarksIconsPath: IPathConfig;
  const AMarkPictureList: IMarkPictureList;
  const AMarkPictureConfig: IMarkPictureConfig
);
begin
  Assert(AMarkPictureList <> nil);
  Assert(AMarkPictureConfig <> nil);

  inherited Create(ALanguageManager);
  FMarksIconsPath := AMarksIconsPath;
  FMarkPictureList := AMarkPictureList;
  FMarkPictureConfig := AMarkPictureConfig;

  LoadList(lstItems.Items);
end;

destructor TfrmMarkPictureConfig.Destroy;
begin
  FreeAndNil(FfrmMarkPictureEditor);
  inherited Destroy;
end;

procedure TfrmMarkPictureConfig.FormCreate(Sender: TObject);
begin
  if lstItems.Count > 0 then begin
    btnEdit.Enabled := True;
    lstItems.Selected[0] := True;
  end else begin
    btnEdit.Enabled := False;
  end;
end;

procedure TfrmMarkPictureConfig.FormResize(Sender: TObject);
begin
  lstItems.Refresh;
end;

procedure TfrmMarkPictureConfig.LoadList(AList: TStrings);
var
  I: Integer;
  VPic: IMarkPicture;
begin
  AList.Clear;
  for I := 0 to FMarkPictureList.GetCount - 1 do begin
    VPic := FMarkPictureList.Get(I);
    AList.AddObject(VPic.GetName, Pointer(VPic));
  end;
end;

procedure DrawIcon(
  const AIcon: IBitmap32Static;
  ACanvas: TCanvas;
  const ABounds: TRect
);
var
  VBitmap: TBitmap32;
  wdth: integer;
  VResampler: TCustomResampler;
begin
  ACanvas.FillRect(ABounds);
  if AIcon <> nil then begin
    wdth := min(ABounds.Right - ABounds.Left, ABounds.Bottom - ABounds.Top);
    VBitmap := TBitmap32.Create;
    try
      VBitmap.SetSize(wdth, wdth);
      VBitmap.Clear(clWhite32);
      VResampler := TLinearResampler.Create;
      try
        StretchTransferFull(
          VBitmap,
          VBitmap.BoundsRect,
          AIcon,
          VResampler,
          dmBlend,
          cmBlend
        );
      finally
        VResampler.Free;
      end;
      VBitmap.DrawTo(ACanvas.Handle, ABounds, VBitmap.BoundsRect);
    finally
      VBitmap.Free;
    end;
  end;
end;

procedure TfrmMarkPictureConfig.lstItemsDrawItem(
  Control: TWinControl;
  Index: Integer;
  Rect: TRect;
  State: TOwnerDrawState
);
const
  cIconSize = 32;
var
  VPic: IMarkPicture;
  VAnchor: TDoublePoint;
  VPoint: TPoint;
  VAnchorName: string;
  VPicFullName: string;
  VBitmap: IBitmap32Static;
begin
  lstItems.Canvas.FillRect(Rect);

  VPic := IMarkPicture(Pointer(lstItems.Items.Objects[Index]));

  // Icon
  VBitmap := VPic.GetMarker;
  if VBitmap <> nil then begin
    DrawIcon(VBitmap, lstItems.Canvas, Bounds(2, Rect.Top + 1, cIconSize, cIconSize));
  end;

  // Name
  lstItems.Canvas.TextOut(cIconSize + 15, Rect.Top + 9, lstItems.Items[Index]);

  // Anchor
  VPicFullName := IncludeTrailingPathDelimiter(FMarksIconsPath.FullPath) + VPic.GetName;
  VAnchor := FMarkPictureConfig.GetAnchor(VPicFullName);

  VAnchorName := '';
  if not IsKnownAnchor(VAnchor, VAnchorName) then begin
    VPoint := AnchorRelativeToPoint(VAnchor, VBitmap.Size);
    VAnchorName := Format('[X: %d; Y: %d]', [VPoint.X, VPoint.Y]);
  end;

  lstItems.Canvas.TextOut(
    Rect.Right - lstItems.Canvas.TextWidth(VAnchorName) - 15,
    Rect.Top + 9,
    VAnchorName
  );

  // line
  lstItems.Canvas.Pen.Color := clSilver;
  lstItems.Canvas.MoveTo(0, Rect.Bottom - 1);
  lstItems.Canvas.LineTo(Rect.Right, Rect.Bottom - 1);
end;

procedure TfrmMarkPictureConfig.lstItemsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    DoEditMarkPicture;
  end;
end;

procedure TfrmMarkPictureConfig.lstItemsDblClick(Sender: TObject);
begin
  DoEditMarkPicture;
end;

procedure TfrmMarkPictureConfig.btnEditClick(Sender: TObject);
begin
  DoEditMarkPicture;
end;

procedure TfrmMarkPictureConfig.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMarkPictureConfig.DoEditMarkPicture;
var
  VIndex: Integer;
  VPic: IMarkPicture;
begin
  VIndex := lstItems.ItemIndex;
  if VIndex >= 0 then begin
    VPic := IMarkPicture(Pointer(lstItems.Items.Objects[VIndex]));
    if FfrmMarkPictureEditor = nil then begin
      FfrmMarkPictureEditor := TfrmMarkPictureEditor.Create(
        Self.LanguageManager,
        FMarksIconsPath,
        FMarkPictureConfig
      );
    end;
    FfrmMarkPictureEditor.EditMarkPicture(VPic);
    lstItems.Repaint;
  end;
end;

end.
