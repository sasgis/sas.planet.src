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
    procedure RefreshTranslation; override;
  end;

implementation

uses
  Menus,
  UResStrings,
  frm_ShortCutEdit;

{$R *.dfm}

procedure TfrShortCutList.lstShortCutListDblClick(Sender: TObject);

  function ShortCutExists(A:TShortcut):Boolean;
  var i:Integer;
  begin
    Result := False;
    for i := 0 to lstShortCutList.Items.Count-1 do begin
      if TShortCutInfo(lstShortCutList.Items.Objects[i]).ShortCut = A then begin
        Result := True;
        Break;
      end;
    end;
  end;

begin
  if lstShortCutList.ItemIndex<>-1 then begin
    frmShortCutEdit.HotKey.HotKey := TShortCutInfo(lstShortCutList.Items.Objects[lstShortCutList.ItemIndex]).ShortCut;
    if frmShortCutEdit.ShowModal = mrOK then begin
      if (ShortCutExists(frmShortCutEdit.HotKey.HotKey))and(frmShortCutEdit.HotKey.HotKey<>0) then begin
        ShowMessage(SAS_MSG_HotKeyExists)
      end else begin
        TShortCutInfo(lstShortCutList.Items.Objects[lstShortCutList.ItemIndex]).ShortCut := frmShortCutEdit.HotKey.HotKey;
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
begin
  with lstShortCutList.Canvas do begin
    FillRect(Rect);
    VTempShortCut := TShortCutInfo(lstShortCutList.Items.Objects[Index]);
    ShortCut := ShortCutToText(VTempShortCut.ShortCut);
    if VTempShortCut.Bitmap <> nil then begin
      CopyRect(Bounds(2,Rect.Top+1,18,18),VTempShortCut.Bitmap.Canvas,bounds(0,0,VTempShortCut.Bitmap.Width,VTempShortCut.Bitmap.Height));
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
