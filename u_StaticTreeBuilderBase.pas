unit u_StaticTreeBuilderBase;

interface

uses
  Classes,
  i_StaticTreeItem,
  i_StaticTreeBuilder;

type
  TStaticTreeBuilderBase = class(TInterfacedObject, IStaticTreeBuilder)
  protected
    procedure ProcessItems(ASource: IInterface; AList: TStringList); virtual; abstract;
    procedure ProcessItem(ASource: IInterface; AItem: IInterface; AList: TStringList);
    function GetNameFromItem(ASource: IInterface; AItem: IInterface): string; virtual; abstract;
    function GetLevelName(const AName: string; out ACurLevelName, ATrailName: string): Boolean; virtual; abstract;
    procedure GetGroupAndVisibleName(const AName: string; out AGroupName, AVisibleName: string); virtual; abstract;
    procedure AddItemToList(AItem: IInterface; AName: string; AList: TStringList);
    function BuildTreeItemsList(AList: TStringList): IInterfaceList;
  protected
    function BuildStatic(ASource: IInterface): IStaticTreeItem;
  end;

  TStaticTreeBuilderBaseBySlash = class(TStaticTreeBuilderBase)
  private
    FLevelsSeparator: string;
    FLevelsSeparatorLen: Integer;
    FGroupSeparator: string;
    FGroupSeparatorLen: Integer;
  protected
    property LevelsSeparator: string read FLevelsSeparator;
    property GroupSeparator: string read FGroupSeparator;
  protected
    function GetLevelName(const AName: string; out ACurLevelName, ATrailName: string): Boolean; override;
    procedure GetGroupAndVisibleName(const AName: string; out AGroupName, AVisibleName: string); override;
  public
    constructor Create(
      ALevelsSeparator: string;
      AGroupSeparator: string
    );
  end;

implementation

uses
  SysUtils,
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

{ TStaticTreeBuilderBase }

procedure TStaticTreeBuilderBase.AddItemToList(AItem: IInterface; AName: string;
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
  if VTrailExists then begin
    AddItemToList(AItem, VTrailName, VTempItem.FSubList);
  end else begin
    if VTempItem.FData = nil then begin
      VTempItem.FData := AItem;
    end;
  end;
end;

function TStaticTreeBuilderBase.BuildStatic(
  ASource: IInterface): IStaticTreeItem;
var
  VTempItem: TTempTreeItem;
begin
  VTempItem := TTempTreeItem.Create;
  try
    ProcessItems(ASource, VTempItem.FSubList);
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

function TStaticTreeBuilderBase.BuildTreeItemsList(
  AList: TStringList): IInterfaceList;
var
  i: Integer;
  VTempItem: TTempTreeItem;
  VTreeItem: IStaticTreeItem;
begin
  Result := nil;
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

procedure TStaticTreeBuilderBase.ProcessItem(
  ASource: IInterface;
  AItem: IInterface;
  AList: TStringList
);
begin
  AddItemToList(AItem, GetNameFromItem(ASource, AItem), AList);
end;

{ TStaticTreeBuilderBaseBySlash }

constructor TStaticTreeBuilderBaseBySlash.Create(ALevelsSeparator,
  AGroupSeparator: string);
begin
  inherited Create;
  FLevelsSeparator := ALevelsSeparator;
  FLevelsSeparatorLen := Length(FLevelsSeparator);
  FGroupSeparator := AGroupSeparator;
  FGroupSeparatorLen := Length(FGroupSeparator);
end;

procedure TStaticTreeBuilderBaseBySlash.GetGroupAndVisibleName(
  const AName: string; out AGroupName, AVisibleName: string);
var
  VPos: Integer;
begin
  if FGroupSeparatorLen > 0 then begin
    VPos := Pos(FGroupSeparator, AName);
    if VPos > 0 then begin
      AVisibleName := Copy(AName, 1, VPos - 1);
      AGroupName := Copy(AName, VPos + FGroupSeparatorLen, Length(AName));
    end else begin
      AVisibleName := AName;
      AGroupName := AName;
    end;
  end else begin
    AVisibleName := AName;
    AGroupName := AName;
  end;
end;

function TStaticTreeBuilderBaseBySlash.GetLevelName(const AName: string;
  out ACurLevelName, ATrailName: string): Boolean;
var
  VPos: Integer;
begin
  if FLevelsSeparatorLen > 0 then begin
    VPos := Pos(FLevelsSeparator, AName);
    if VPos > 0 then begin
      ACurLevelName := Copy(AName, 1, VPos - 1);
      ATrailName := Copy(AName, VPos + FLevelsSeparatorLen, Length(AName));
      Result := True;
    end else begin
      ACurLevelName := AName;
      ATrailName := '';
      Result := False;
    end;
  end else begin
    ACurLevelName := AName;
    ATrailName := '';
    Result := False;
  end;
end;

end.
