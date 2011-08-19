unit u_TreeByMapActiveMapsSet;

interface

uses
  i_JclNotify,
  i_StaticTreeItem,
  i_TreeChangeable,
  i_ActiveMapsConfig;

type
  TTreeByMapActiveMapsSet = class(TInterfacedObject, ITreeChangeable)
  private
    FMapsSet: IActiveMapsSet;
    FStaticTree: IStaticTreeItem;
    FChangeNotifier: IJclNotifier;
  protected
    function CreateStatic: IStaticTreeItem;
  protected
    function GetStatic: IStaticTreeItem;
    function GetChangeNotifier: IJclNotifier;
  public
    constructor Create(AMapsSet: IActiveMapsSet);
  end;

implementation

uses
  u_JclNotify;

{ TTreeByMapActiveMapsSet }

constructor TTreeByMapActiveMapsSet.Create(AMapsSet: IActiveMapsSet);
begin
  FMapsSet := AMapsSet;
  FChangeNotifier := TJclBaseNotifier.Create;
  FStaticTree := CreateStatic;
end;

function TTreeByMapActiveMapsSet.GetChangeNotifier: IJclNotifier;
begin
  Result := FChangeNotifier;
end;

function TTreeByMapActiveMapsSet.GetStatic: IStaticTreeItem;
begin
  Result := FStaticTree;
end;

end.
