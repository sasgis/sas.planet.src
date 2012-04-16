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
  i_ShortCutSingleConfig,
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
      const ALanguageManager: ILanguageManager
    ); reintroduce;
    procedure SetShortCutManager(AShortCutManager: TShortcutManager);
    procedure CancelChanges;
    procedure ApplyChanges;
    procedure RefreshTranslation; override;
  end;

implementation

uses
  Menus,
  u_ShortCutModalEditByForm,
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
  const ALanguageManager: ILanguageManager
);
begin
  inherited Create(nil);
  FShortCutEdit := TShortCutModalEditByForm.Create(ALanguageManager);
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

procedure TfrShortCutList.lstShortCutListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ShortCut:String;
  VTempShortCut: IShortCutSingleConfig;
  VBitmap: TBitmap;
begin
  with lstShortCutList.Canvas do begin
    FillRect(Rect);
    VTempShortCut := IShortCutSingleConfig(Pointer(lstShortCutList.Items.Objects[Index]));
    ShortCut := ShortCutToText(VTempShortCut.ShortCut);
    VBitmap := VTempShortCut.IconBitmap;
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
    LoadList(lstShortCutList.Items);
  end;
end;

procedure TfrShortCutList.SetShortCutManager(
  AShortCutManager: TShortcutManager);
begin
  if FShortCutManager <> AShortCutManager then begin
    FShortCutManager := AShortCutManager;
    if FShortCutManager <> nil then begin
      LoadList(lstShortCutList.Items);
    end;
  end;
end;

end.
