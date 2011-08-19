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
    FDescription: string;
    FSubItems: TInterfaceList;
  protected
    function GetData: IInterface;
    function GetName: string;
    function GetDescription: string;
    function GetSubItemCount: Integer;
    function GetSubItem(AIndex: Integer): IStaticTreeItem;
  public
    constructor Create(
      AData: IInterface;
      AName: string;
      ADescription: string;
      ASubItems: TInterfaceList
    );
  end;

implementation

{ TStaticTreeItem }

constructor TStaticTreeItem.Create(AData: IInterface; AName,
  ADescription: string; ASubItems: TInterfaceList);
begin
  FData := AData;
  FName := AName;
  FDescription := ADescription;
  FSubItems := ASubItems;
end;

function TStaticTreeItem.GetData: IInterface;
begin
  Result := FData;
end;

function TStaticTreeItem.GetDescription: string;
begin
  Result := FDescription;
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
