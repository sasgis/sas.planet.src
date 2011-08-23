unit u_StaticTreeItem;

interface

uses
  Classes,
  i_StaticTreeItem;

type
  TStaticTreeItem = class(TInterfacedObject, IStaticTreeItem)
  private
    FData: IInterface;
    FName: string;
    FGroupName: string;
    FSubItems: IInterfaceList;
  protected
    function GetData: IInterface;
    function GetName: string;
    function GetGroupName: string;
    function GetSubItemCount: Integer;
    function GetSubItem(AIndex: Integer): IStaticTreeItem;
  public
    constructor Create(
      AData: IInterface;
      AName: string;
      AGroupName: string;
      ASubItems: IInterfaceList
    );
  end;

implementation

{ TStaticTreeItem }

constructor TStaticTreeItem.Create(AData: IInterface; AName,
  AGroupName: string; ASubItems: IInterfaceList);
begin
  FData := AData;
  FName := AName;
  FGroupName := AGroupName;
  FSubItems := ASubItems;
end;

function TStaticTreeItem.GetData: IInterface;
begin
  Result := FData;
end;

function TStaticTreeItem.GetGroupName: string;
begin
  Result := FGroupName;
end;

function TStaticTreeItem.GetName: string;
begin
  Result := FName;
end;

function TStaticTreeItem.GetSubItem(AIndex: Integer): IStaticTreeItem;
begin
  Result := nil;
  if FSubItems <> nil then begin
    if (AIndex >= 0) and (AIndex < FSubItems.Count) then begin
      Result := IStaticTreeItem(FSubItems.Items[AIndex]);
    end;
  end;
end;

function TStaticTreeItem.GetSubItemCount: Integer;
begin
  if FSubItems <> nil then begin
    Result := FSubItems.Count;
  end else begin
    Result := 0;
  end;
end;

end.
