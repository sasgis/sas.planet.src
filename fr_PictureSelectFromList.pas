{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit fr_PictureSelectFromList;

interface

uses
  Windows,
  Types,
  Classes,
  Graphics,
  Controls,
  Forms,
  Grids,
  GR32,
  i_MarkPicture,
  i_LanguageManager,
  u_CommonFormAndFrameParents;

type
  TfrPictureSelectFromList = class(TFrame)
    drwgrdIcons: TDrawGrid;
    procedure drwgrdIconsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect:
        TRect; State: TGridDrawState);
    procedure drwgrdIconsKeyDown(Sender: TObject; var Key: Word; Shift:
        TShiftState);
    procedure drwgrdIconsMouseMove(Sender: TObject; Shift: TShiftState; X, Y:
        Integer);
    procedure drwgrdIconsMouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure FrameEnter(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    FPictureList: IMarkPictureList;
    FOnSelect: TNotifyEvent;
    FPicture: IMarkPicture;
    procedure DrawFromMarkIcons(
      ACanvas: TCanvas;
      const APic: IMarkPicture;
      const ASize: Integer;
      const bound: TRect
    );
    procedure SetPicture(const Value: IMarkPicture);
  public
    property Picture: IMarkPicture read FPicture write SetPicture;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const APictureList: IMarkPictureList;
      AOnSelect: TNotifyEvent
    ); reintroduce;
  end;

implementation

uses
  Math,
  GR32_Resamplers,
  i_BitmapMarker,
  u_BitmapFunc;

{$R *.dfm}

constructor TfrPictureSelectFromList.Create(
  const ALanguageManager: ILanguageManager;
  const APictureList: IMarkPictureList;
  AOnSelect: TNotifyEvent
);
begin
  inherited Create(ALanguageManager);
  FPictureList := APictureList;
  FOnSelect := AOnSelect;
end;

procedure TfrPictureSelectFromList.DrawFromMarkIcons(
  ACanvas: TCanvas;
  const APic: IMarkPicture;
  const ASize: Integer;
  const bound: TRect
);
var
  VBitmap: TBitmap32;
  VResampler: TCustomResampler;
  VMarker: IBitmapMarker;
  VSourceRect: TRect;
  VSourceSize: TPoint;
  VDstRect: TRect;
  VScale: Double;
begin
  VMarker := nil;
  if APic <> nil then begin
    VMarker := APic.GetMarker;
  end;
  if VMarker <> nil then begin
    VBitmap:=TBitmap32.Create;
    try
      VBitmap.SetSize(bound.Right-bound.Left, bound.Bottom-bound.Top);
      VBitmap.Clear(clWhite32);
      VSourceSize := VMarker.Size;
      VScale := Min(VBitmap.Width / VSourceSize.X, VBitmap.Height / VSourceSize.Y);
      VSourceRect := Rect(0, 0, VSourceSize.X, VSourceSize.Y);
      VDstRect :=
        Rect(
          Trunc((VBitmap.Width - VSourceSize.X * VScale) / 2),
          Trunc((VBitmap.Height - VSourceSize.Y * VScale) / 2),
          Trunc((VBitmap.Width + VSourceSize.X * VScale) / 2),
          Trunc((VBitmap.Height + VSourceSize.Y * VScale) / 2)
        );
      VResampler := TLinearResampler.Create;
      try
        StretchTransfer(
          VBitmap,
          VDstRect,
          VMarker,
          VSourceRect,
          VResampler,
          dmBlend,
          cmBlend
        );
      finally
        VResampler.Free;
      end;
      VBitmap.DrawTo(ACanvas.Handle, bound, VBitmap.BoundsRect);
    finally
      VBitmap.Free;
    end;
  end else begin
    ACanvas.FillRect(bound);
  end;
end;

procedure TfrPictureSelectFromList.drwgrdIconsDrawCell(Sender: TObject; ACol, ARow: Integer;
    Rect: TRect; State: TGridDrawState);
var
  i:Integer;
  VPictureList: IMarkPictureList;
begin
  i := (ARow * drwgrdIcons.ColCount) + ACol;
  VPictureList := FPictureList;
  if i < VPictureList.Count then
    DrawFromMarkIcons(drwgrdIcons.Canvas, VPictureList.Get(i), drwgrdIcons.DefaultColWidth, Rect);
end;

procedure TfrPictureSelectFromList.drwgrdIconsKeyDown(Sender: TObject; var Key:
    Word; Shift: TShiftState);
var
  i:integer;
begin
  if Key = VK_SPACE then begin
    i:=(drwgrdIcons.Row*drwgrdIcons.ColCount)+drwgrdIcons.Col;
    if (i >= 0) and (i < FPictureList.Count) then begin
      FPicture := FPictureList.Get(i);
      FOnSelect(Self);
    end;
  end;
end;

procedure TfrPictureSelectFromList.drwgrdIconsMouseMove(Sender: TObject; Shift:
    TShiftState; X, Y: Integer);
var
  i:integer;
  ACol,ARow: Integer;
begin
  drwgrdIcons.MouseToCell(X, Y, ACol, ARow);
  i:=(ARow*drwgrdIcons.ColCount)+ACol;
  if (ARow>-1)and(ACol>-1) and (i < FPictureList.Count) then begin
    drwgrdIcons.Hint := FPictureList.Get(i).GetName;
  end;
end;

procedure TfrPictureSelectFromList.drwgrdIconsMouseUp(Sender: TObject; Button:
    TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i:integer;
  ACol,ARow: Integer;
begin
  drwgrdIcons.MouseToCell(X, Y, ACol, ARow);
  i:=(ARow*drwgrdIcons.ColCount)+ACol;
  if (ARow>-1)and(ACol>-1) and (i < FPictureList.Count) then begin
    FPicture := FPictureList.Get(i);
    FOnSelect(Self);
  end;
end;

procedure TfrPictureSelectFromList.FrameEnter(Sender: TObject);
begin
  drwgrdIcons.SetFocus;
end;

procedure TfrPictureSelectFromList.FrameResize(Sender: TObject);
var
  VPicCount: Integer;
  VColCount: Integer;
  VRowCount: Integer;
  VIndex: Integer;
begin
  VPicCount := FPictureList.Count;
  VColCount := Trunc(drwgrdIcons.ClientWidth / drwgrdIcons.DefaultColWidth);
  drwgrdIcons.ColCount := VColCount;
  VRowCount := VPicCount div VColCount;
  if (VPicCount mod VColCount) > 0 then begin
    Inc(VRowCount);
  end;
  drwgrdIcons.RowCount := VRowCount;

  VIndex := -1;
  if Assigned(FPicture) then begin
    VIndex := FPictureList.GetIndexByName(FPicture.GetName);
  end;
  if VIndex >= 0 then begin
    drwgrdIcons.Row := VIndex div drwgrdIcons.ColCount;
    drwgrdIcons.Col := VIndex mod drwgrdIcons.ColCount;
  end else begin
    drwgrdIcons.Row := 0;
    drwgrdIcons.Col := 0;
  end;
  drwgrdIcons.Repaint;
end;

procedure TfrPictureSelectFromList.SetPicture(const Value: IMarkPicture);
var
  VIndex: Integer;
  VCol: Integer;
  VRow: Integer;
begin
  FPicture := Value;
  VIndex := -1;
  if Assigned(FPicture) then begin
    VIndex := FPictureList.GetIndexByName(FPicture.GetName);
  end;
  if VIndex >= 0 then begin
    VRow := VIndex div drwgrdIcons.ColCount;
    VCol := VIndex mod drwgrdIcons.ColCount;
    if (VRow < drwgrdIcons.RowCount) and (VCol < drwgrdIcons.ColCount) then begin
      drwgrdIcons.Row := VRow;
      drwgrdIcons.Col := VCol;
    end;
  end else begin
    drwgrdIcons.Row := 0;
    drwgrdIcons.Col := 0;
  end;
end;

end.
