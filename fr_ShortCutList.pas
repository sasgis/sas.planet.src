{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit fr_ShortCutList;

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
  StdCtrls,
  ExtCtrls,
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
    FShortCutManager: TShortcutManager;
  public
    procedure SetShortCutManager(AShortCutManager: TShortcutManager);
    procedure CancelChanges;
    procedure ApplyChanges;
    procedure RefreshTranslation; override;
  end;

implementation

uses
  Menus,
  u_ResStrings,
  frm_ShortCutEdit;

{$R *.dfm}

procedure TfrShortCutList.ApplyChanges;
begin
  FShortCutManager.ApplyChanges;
end;

procedure TfrShortCutList.CancelChanges;
begin
  FShortCutManager.CancelChanges;
end;

procedure TfrShortCutList.lstShortCutListDblClick(Sender: TObject);
var
  VTempShortCut: TShortCutInfo;
  VExistsShortCut: TShortCutInfo;
begin
  if lstShortCutList.ItemIndex<>-1 then begin
    VTempShortCut := TShortCutInfo(lstShortCutList.Items.Objects[lstShortCutList.ItemIndex]);
    if frmShortCutEdit.EditHotKeyModal(VTempShortCut) then begin
      VExistsShortCut := FShortCutManager.GetShortCutInfoByShortCut(VTempShortCut.ShortCut);
      if (VExistsShortCut <> nil) and (VExistsShortCut <> VTempShortCut) then begin
        VTempShortCut.ResetShortCut;
        ShowMessage(SAS_MSG_HotKeyExists);
      end;
      lstShortCutList.Repaint;
    end;
  end;
end;

procedure TfrShortCutList.lstShortCutListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ShortCut:String;
  VTempShortCut: TShortCutInfo;
  VBitmap: TBitmap;
begin
  with lstShortCutList.Canvas do begin
    FillRect(Rect);
    VTempShortCut := TShortCutInfo(lstShortCutList.Items.Objects[Index]);
    ShortCut := ShortCutToText(VTempShortCut.ShortCut);
    VBitmap := VTempShortCut.Bitmap;
    if VBitmap <> nil then begin
      CopyRect(Bounds(2,Rect.Top+1,18,18),VBitmap.Canvas,bounds(0,0,VBitmap.Width,VBitmap.Height));
    end;
    TextOut(22,Rect.Top+3, lstShortCutList.Items[Index]);
    TextOut(Rect.Right-TextWidth(ShortCut)-9,Rect.Top+3, ShortCut);

    Pen.Color := clSilver;
    MoveTo(0, Rect.Bottom-1);
    LineTo(Rect.Right, Rect.Bottom-1);
  end;
end;

procedure TfrShortCutList.RefreshTranslation;
begin
  inherited;
  if FShortCutManager <> nil then begin
    FShortCutManager.CancelChanges;
    FShortCutManager.GetObjectsList(lstShortCutList.Items);
  end;
end;

procedure TfrShortCutList.SetShortCutManager(
  AShortCutManager: TShortcutManager);
begin
  if FShortCutManager <> AShortCutManager then begin
    FShortCutManager := AShortCutManager;
    if FShortCutManager <> nil then begin
      FShortCutManager.GetObjectsList(lstShortCutList.Items);
    end;
  end;
end;

end.
