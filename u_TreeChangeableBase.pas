unit u_TreeChangeableBase;

interface

uses
  Classes,
  i_JclNotify,
  i_StaticTreeItem,
  i_TreeChangeable;

type
  TTreeChangeableBase = class(TInterfacedObject, ITreeChangeable)
  private
    FStaticTree: IStaticTreeItem;
    FConfigChangeListener: IJclListener;
    FConfigChangeNotifier: IJclNotifier;
    FChangeNotifier: IJclNotifier;
    procedure OnConfigChange(Sender: TObject);
  protected
    function CreateStatic: IStaticTreeItem;
    procedure ProcessItems(AList: TStringList); virtual; abstract;
    procedure ProcessItem(AItem: IInterface; AList: TStringList);
    function GetNameFromItem(AItem: IInterface): string; virtual; abstract;
    function GetLevelName(const AName: string; out ACurLevelName, ATrailName: string): Boolean; virtual; abstract;
    procedure GetGroupAndVisibleName(const AName: string; out AGroupName, AVisibleName: string); virtual; abstract;
    procedure AddItemToList(AItem: IInterface; AName: string; AList: TStringList);
    function BuildTreeItemsList(AList: TStringList): IInterfaceList;
  protected
    function GetStatic: IStaticTreeItem;
    function GetChangeNotifier: IJclNotifier;
  public
    constructor Create(AConfigChangeNotifier: IJclNotifier);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_JclNotify,
  u_NotifyEventListener,
  u_StaticTreeItem;

type
  TTempTreeItem = class
  private
    FSubList: TStringList;
    FData: IInterface;
    FVisibleName: string;
    FGroupName: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TTempTreeItem }

constructor TTempTreeItem.Create;
begin
  FSubList := TStringList.Create;
  FSubList.Sorted := True;
  FSubList.Duplicates := dupError;
end;

destructor TTempTreeItem.Destroy;
var
  i: Integer;
  VObj: TObject;
begin
  for i := 0 to FSubList.Count - 1 do begin
    VObj := FSubList.Objects[i];
    FSubList.Objects[i] := nil;
    VObj.Free;
  end;
  FreeAndNil(FSubList);
  FData := nil;
  inherited;
end;

{ TTreeChangeableBase }

constructor TTreeChangeableBase.Create(AConfigChangeNotifier: IJclNotifier);
begin
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

procedure TTreeChangeableBase.AddItemToList(AItem: IInterface; AName: string;
  AList: TStringList);
var
  VCurLevelName: string;
  VTrailName: string;
  VIndex: Integer;
  VTempItem: TTempTreeItem;
  VGroupName: string;
  VVisibleName: string;
  VTrailExists: Boolean;
begin
  VTrailExists := GetLevelName(AName, VCurLevelName, VTrailName);
  GetGroupAndVisibleName(VCurLevelName, VGroupName, VVisibleName);
  if AList.Find(VGroupName, VIndex) then begin
    VTempItem := TTempTreeItem(AList.Objects[VIndex]);
  end else begin
    VTempItem := TTempTreeItem.Create;
    AList.AddObject(VGroupName, VTempItem);
  end;
  if VTempItem.FGroupName = '' then begin
    VTempItem.FGroupName := VGroupName;
  end;
  if VTempItem.FVisibleName = '' then begin
    VTempItem.FVisibleName := VVisibleName;
  end;
  if VTempItem.FData = nil then begin
    VTempItem.FData := AItem;
  end;
  if VTrailExists then begin
    AddItemToList(AItem, VTrailName, VTempItem.FSubList);
  end;
end;

function TTreeChangeableBase.BuildTreeItemsList(
  AList: TStringList): IInterfaceList;
var
  i: Integer;
  VTempItem: TTempTreeItem;
  VTreeItem: IStaticTreeItem;
begin
  if AList.Count > 0 then begin
    Result := TInterfaceList.Create;
    for i := 0 to AList.Count - 1 do begin
      VTempItem := TTempTreeItem(AList.Objects[i]);
      VTreeItem :=
        TStaticTreeItem.Create(
          VTempItem.FData,
          VTempItem.FVisibleName,
          VTempItem.FGroupName,
          BuildTreeItemsList(VTempItem.FSubList)
        );
      Result.Add(VTreeItem);
    end;
  end;
end;

procedure TTreeChangeableBase.ProcessItem(AItem: IInterface;
  AList: TStringList);
begin
  AddItemToList(AItem, GetNameFromItem(AItem), AList);
end;

function TTreeChangeableBase.CreateStatic: IStaticTreeItem;
var
  VTempItem: TTempTreeItem;
begin
  VTempItem := TTempTreeItem.Create;
  try
    ProcessItems(VTempItem.FSubList);
    Result :=
      TStaticTreeItem.Create(
        nil,
        '',
        '',
        BuildTreeItemsList(VTempItem.FSubList)
      );
  finally
    VTempItem.Free;
  end;
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
