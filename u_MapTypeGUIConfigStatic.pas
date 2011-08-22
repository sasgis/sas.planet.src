unit u_MapTypeGUIConfigStatic;

interface

uses
  i_MapTypeGUIConfig;

type
  TMapTypeGUIConfigStatic = class(TInterfacedObject, IMapTypeGUIConfigStatic)
  private
    FName: string;
    FSortIndex: Integer;
    FSeparator: Boolean;
    FParentSubMenu: string;
    FEnabled: Boolean;
  protected
    function GetName: string;
    function GetSortIndex: Integer;
    function GetSeparator: Boolean;
    function GetParentSubMenu: string;
    function GetEnabled: Boolean;
  public
    constructor Create(
      AName: string;
      ASortIndex: Integer;
      ASeparator: Boolean;
      AParentSubMenu: string;
      AEnabled: Boolean
    );
  end;

implementation

{ TMapTypeGUIConfigStatic }

constructor TMapTypeGUIConfigStatic.Create(AName: string; ASortIndex: Integer;
  ASeparator: Boolean; AParentSubMenu: string; AEnabled: Boolean);
begin
  FName := AName;
  FSortIndex := ASortIndex;
  FSeparator := ASeparator;
  FParentSubMenu := AParentSubMenu;
  FEnabled :=  AEnabled;
end;

function TMapTypeGUIConfigStatic.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TMapTypeGUIConfigStatic.GetName: string;
begin
  Result := FName;
end;

function TMapTypeGUIConfigStatic.GetParentSubMenu: string;
begin
  Result := FParentSubMenu;
end;

function TMapTypeGUIConfigStatic.GetSeparator: Boolean;
begin
  Result := FSeparator;
end;

function TMapTypeGUIConfigStatic.GetSortIndex: Integer;
begin
  Result := FSortIndex;
end;

end.
