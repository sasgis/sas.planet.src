unit u_MenuGeneratorByStaticTreeSimple;

interface

uses
  Classes,
  TB2Item,
  i_StaticTreeItem,
  i_MenuGeneratorByTree;

type
  TMenuGeneratorByStaticTreeSimple = class(TInterfacedObject, IMenuGeneratorByTree)
  private
    FOnClick: TNotifyEvent;
  protected
    procedure AddItem(AParent: TTBCustomItem; AItem: IStaticTreeItem); virtual;
    procedure ClearOldItems(ARootMenu: TTBCustomItem); virtual;
    function IsFlatSubTree(AItem: IStaticTreeItem): Boolean; virtual;
  protected
    procedure BuildMenu(
      ARootMenu: TTBCustomItem;
      ATree: IStaticTreeItem
    );
  public
    constructor Create(
      AOnClick: TNotifyEvent
    );
  end;
implementation

uses
  TBX,
  TBXExtItems;

{ TMenuGeneratorByStaticTreeSimple }

constructor TMenuGeneratorByStaticTreeSimple.Create(AOnClick: TNotifyEvent);
begin
  FOnClick := AOnClick;
end;

procedure TMenuGeneratorByStaticTreeSimple.AddItem(AParent: TTBCustomItem;
  AItem: IStaticTreeItem);
var
  VItem: TTBCustomItem;
  i: Integer;
begin
  if AItem.SubItemCount > 0 then begin
    if IsFlatSubTree(AItem) then begin
      if Length(AItem.Name) > 0 then begin
        VItem := TTBXLabelItem.Create(AParent);
        VItem.Caption := AItem.Name;
        VItem.Tag := -1;
        AParent.Add(VItem);
        for i := 0 to AItem.SubItemCount - 1 do begin
          AddItem(AParent, AItem.SubItem[i]);
        end;
      end else begin
        for i := 0 to AItem.SubItemCount - 1 do begin
          AddItem(AParent, AItem.SubItem[i]);
        end;
        VItem := TTBSeparatorItem.Create(AParent);
        VItem.Tag := -1;
        AParent.Add(VItem);
      end;
    end else begin
      VItem := TTBXSubmenuItem.Create(AParent);
      VItem.Caption := AItem.Name;
      VItem.Tag := -1;
      AParent.Add(VItem);
      for i := 0 to AItem.SubItemCount - 1 do begin
        AddItem(AParent, AItem.SubItem[i]);
      end;
    end;
  end else begin
    if AItem.Data <> nil then begin
      VItem := TTBXItem.Create(AParent);
      VItem.Caption := AItem.Name;
      VItem.Tag := Integer(AItem.Data);
      VItem.OnClick := FOnClick;
      AParent.Add(VItem);
    end;
  end;

end;

procedure TMenuGeneratorByStaticTreeSimple.BuildMenu(ARootMenu: TTBCustomItem;
  ATree: IStaticTreeItem);
begin
  ClearOldItems(ARootMenu);
  AddItem(ARootMenu, ATree);
end;

procedure TMenuGeneratorByStaticTreeSimple.ClearOldItems(ARootMenu: TTBCustomItem);
var
  i: integer;
begin
  for i := ARootMenu.Count - 1 downto 0 do begin
    if ARootMenu.Items[i].Tag <> 0 then begin
      ARootMenu.Items[i].Free;
    end;
  end;
end;

function TMenuGeneratorByStaticTreeSimple.IsFlatSubTree(
  AItem: IStaticTreeItem): Boolean;
begin
  Result := AItem.GroupName[Length(AItem.GroupName)] = '~';
end;

end.
