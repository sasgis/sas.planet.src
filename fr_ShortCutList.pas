{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit fr_ShortCutList;

interface

uses
  Windows,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  i_LanguageManager,
  i_ShortCutModalEdit,
  u_CommonFormAndFrameParents,
  u_ShortcutManager;

type
  TfrShortCutList = class(TFrame)
    lstShortCutList: TListBox;
    pnlHotKeysHeader: TPanel;
    lblOperation: TLabel;
    lblHotKey: TLabel;
    procedure lstShortCutListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lstShortCutListDblClick(Sender: TObject);
  private
    FShortCutEdit: IShortCutModalEdit;
    FShortCutManager: TShortcutManager;
    procedure LoadList(AList: TStrings);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      AShortCutManager: TShortcutManager
    ); reintroduce;
    procedure CancelChanges;
    procedure ApplyChanges;
    procedure RefreshTranslation; override;
  end;

implementation

uses
  Menus,
  Math,
  GR32,
  GR32_Resamplers,
  i_Bitmap32Static,
  i_ShortCutSingleConfig,
  u_ShortCutModalEditByForm,
  u_BitmapFunc,
  u_ResStrings;

{$R *.dfm}

procedure TfrShortCutList.ApplyChanges;
begin
  FShortCutManager.ApplyChanges;
end;

procedure TfrShortCutList.CancelChanges;
begin
  FShortCutManager.CancelChanges;
end;

constructor TfrShortCutList.Create(
  const ALanguageManager: ILanguageManager;
  AShortCutManager: TShortcutManager
);
begin
  inherited Create(ALanguageManager);
  FShortCutManager := AShortCutManager;
  FShortCutEdit := TShortCutModalEditByForm.Create(ALanguageManager);

  if FShortCutManager <> nil then begin
    LoadList(lstShortCutList.Items);
  end;
end;

procedure TfrShortCutList.LoadList(AList: TStrings);
var
  i: Integer;
  VShortCutInfo: IShortCutSingleConfig;
begin
  AList.Clear;
  for i := 0 to FShortCutManager.GetCount - 1 do begin
    VShortCutInfo := FShortCutManager.GetItem(i);
    AList.AddObject(VShortCutInfo.GetCaption, Pointer(VShortCutInfo));
  end;
end;

procedure TfrShortCutList.lstShortCutListDblClick(Sender: TObject);
var
  VTempShortCut: IShortCutSingleConfig;
  VExistsShortCut: IShortCutSingleConfig;
  VIndex: Integer;
begin
  VIndex := lstShortCutList.ItemIndex;
  if VIndex >= 0 then begin
    VTempShortCut := IShortCutSingleConfig(Pointer(lstShortCutList.Items.Objects[VIndex]));
    if FShortCutEdit.EditShortCut(VTempShortCut) then begin
      VExistsShortCut := FShortCutManager.GetShortCutInfoByShortCut(VTempShortCut.ShortCut);
      if (VExistsShortCut <> nil) and (VExistsShortCut <> VTempShortCut) then begin
        VTempShortCut.ResetShortCut;
        ShowMessage(SAS_MSG_HotKeyExists);
      end;
      lstShortCutList.Repaint;
    end;
  end;
end;

procedure DrawIcon(
  const AIcon: IBitmap32Static;
  ACanvas: TCanvas;
  const ABounds:TRect
);
var
  VBitmap: TBitmap32;
  wdth:integer;
  VResampler: TCustomResampler;
begin
  ACanvas.FillRect(ABounds);
  if AIcon <> nil then begin
    wdth:=min(ABounds.Right-ABounds.Left,ABounds.Bottom-ABounds.Top);
    VBitmap:=TBitmap32.Create;
    try
      VBitmap.SetSize(wdth,wdth);
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

procedure TfrShortCutList.lstShortCutListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ShortCut:String;
  VTempShortCut: IShortCutSingleConfig;
  VBitmap: IBitmap32Static;
begin
  lstShortCutList.Canvas.FillRect(Rect);
  VTempShortCut := IShortCutSingleConfig(Pointer(lstShortCutList.Items.Objects[Index]));
  ShortCut := ShortCutToText(VTempShortCut.ShortCut);
  VBitmap := VTempShortCut.IconBitmap;
  if VBitmap <> nil then begin
    DrawIcon(VBitmap, lstShortCutList.Canvas, Bounds(2,Rect.Top+1,18,18));
  end;
  lstShortCutList.Canvas.TextOut(22,Rect.Top+3, lstShortCutList.Items[Index]);
  lstShortCutList.Canvas.TextOut(Rect.Right-lstShortCutList.Canvas.TextWidth(ShortCut)-9,Rect.Top+3, ShortCut);

  lstShortCutList.Canvas.Pen.Color := clSilver;
  lstShortCutList.Canvas.MoveTo(0, Rect.Bottom-1);
  lstShortCutList.Canvas.LineTo(Rect.Right, Rect.Bottom-1);
end;

procedure TfrShortCutList.RefreshTranslation;
begin
  inherited;
  if FShortCutManager <> nil then begin
    FShortCutManager.CancelChanges;
    LoadList(lstShortCutList.Items);
  end;
end;

end.
