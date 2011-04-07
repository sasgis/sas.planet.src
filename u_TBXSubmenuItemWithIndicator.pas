unit u_TBXSubmenuItemWithIndicator;

interface

uses
  Graphics,
  Classes,
  TB2Item,
  TBX;

type
  TTBXSubmenuItemWithIndicator = class(TTBXSubmenuItem)
  private
    procedure AdjustFont(Item: TTBCustomItem;
      Viewer: TTBItemViewer; Font: TFont; StateFlags: Integer);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TTBXSubmenuItemWithIndicator }

constructor TTBXSubmenuItemWithIndicator.Create(AOwner: TComponent);
begin
  inherited;
  OnAdjustFont := Self.AdjustFont;
end;

procedure TTBXSubmenuItemWithIndicator.AdjustFont(Item: TTBCustomItem;
  Viewer: TTBItemViewer; Font: TFont; StateFlags: Integer);
var
  VChildSelected: Boolean;
  i: Integer;
begin
  VChildSelected := False;
  for i := 0 to  Self.Count - 1 do begin
    if Self.Items[i].Checked then begin
      VChildSelected := True;
      Break;
    end;
  end;
  if VChildSelected then begin
    Self.FontSettings.Bold := tsTrue;
  end else begin
    Self.FontSettings.Bold := tsDefault;
  end;
end;

end.
