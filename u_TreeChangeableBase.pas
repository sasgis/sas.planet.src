unit u_TreeChangeableBase;

interface

uses
  Classes,
  i_JclNotify,
  i_StaticTreeItem,
  i_StaticTreeBuilder,
  i_TreeChangeable;

type
  TTreeChangeableBase = class(TInterfacedObject, ITreeChangeable)
  private
    FStaticTreeBuilder: IStaticTreeBuilder;
    FStaticTree: IStaticTreeItem;
    FConfigChangeListener: IJclListener;
    FConfigChangeNotifier: IJclNotifier;
    FChangeNotifier: IJclNotifier;
    procedure OnConfigChange(Sender: TObject);
  protected
    function CreateStatic: IStaticTreeItem;
    function GetSource: IInterface; virtual; abstract;
  protected
    function GetStatic: IStaticTreeItem;
    function GetChangeNotifier: IJclNotifier;
  public
    constructor Create(
      AStaticTreeBuilder: IStaticTreeBuilder;
      AConfigChangeNotifier: IJclNotifier
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_JclNotify,
  u_NotifyEventListener;

{ TTreeChangeableBase }

constructor TTreeChangeableBase.Create(
  AStaticTreeBuilder: IStaticTreeBuilder;
  AConfigChangeNotifier: IJclNotifier
);
begin
  FStaticTreeBuilder := AStaticTreeBuilder;
  FConfigChangeNotifier := AConfigChangeNotifier;
  FChangeNotifier := TJclBaseNotifier.Create;
  FConfigChangeListener := TNotifyEventListener.Create(Self.OnConfigChange);
  FConfigChangeNotifier.Add(FConfigChangeListener);
  OnConfigChange(nil);
end;

destructor TTreeChangeableBase.Destroy;
begin
  FConfigChangeNotifier.Remove(FConfigChangeListener);
  FConfigChangeListener := nil;
  FConfigChangeNotifier := nil;

  inherited;
end;

function TTreeChangeableBase.CreateStatic: IStaticTreeItem;
begin
  Result := FStaticTreeBuilder.BuildStatic(GetSource);
end;

function TTreeChangeableBase.GetChangeNotifier: IJclNotifier;
begin
  Result := FChangeNotifier;
end;

function TTreeChangeableBase.GetStatic: IStaticTreeItem;
begin
  Result := FStaticTree;
end;

procedure TTreeChangeableBase.OnConfigChange(Sender: TObject);
begin
  FStaticTree := CreateStatic;
  FChangeNotifier.Notify(nil);
end;

end.
