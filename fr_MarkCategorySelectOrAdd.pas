unit fr_MarkCategorySelectOrAdd;

interface

uses
  Classes,
  Controls,
  Forms,
  StdCtrls,
  u_CommonFormAndFrameParents,
  i_LanguageManager,
  i_MarkCategory,
  i_MarkCategoryDB;

type
  TfrMarkCategorySelectOrAdd = class(TFrame)
    CBKateg: TComboBox;
    lblCategory: TLabel;
  private
    FCategoryDB: IMarkCategoryDB;
    FCategoryList: IInterfaceList;
    FLastUsedCategoryName: string;
    procedure CategoryListToStrings(const AList: IInterfaceList; AStrings: TStrings);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ACategoryDb: IMarkCategoryDB
    ); reintroduce;
    destructor Destroy; override;
    procedure Init(const ACategory: ICategory);
    function GetCategory: ICategory;
    procedure Clear;
  end;

implementation

{$R *.dfm}

{ TfrMarkCategorySelectOrAdd }

constructor TfrMarkCategorySelectOrAdd.Create(
  const ALanguageManager: ILanguageManager;
  const ACategoryDb: IMarkCategoryDB
);
begin
  inherited Create(ALanguageManager);
  FCategoryDB := ACategoryDb;
  FCategoryList := nil;
  FLastUsedCategoryName := '';
end;

destructor TfrMarkCategorySelectOrAdd.Destroy;
begin
  Clear;
  FCategoryDB := nil;
  inherited;
end;

procedure TfrMarkCategorySelectOrAdd.CategoryListToStrings(
  const AList: IInterfaceList;
  AStrings: TStrings
);
var
  i: Integer;
  VCategory: IMarkCategory;
begin
  AStrings.Clear;
  for i := 0 to AList.Count - 1 do begin
    VCategory := IMarkCategory(AList[i]);
    AStrings.AddObject(VCategory.Name, Pointer(VCategory));
  end;
end;

procedure TfrMarkCategorySelectOrAdd.Clear;
begin
  FCategoryList := nil;
  CBKateg.Items.Clear;
end;

function TfrMarkCategorySelectOrAdd.GetCategory: ICategory;
var
  VIndex: Integer;
  VCategoryText: string;
  VCategory: IMarkCategory;
begin
  Result := nil;
  VCategoryText := CBKateg.Text;
  VIndex := CBKateg.ItemIndex;
  if VIndex < 0 then begin
    VIndex:= CBKateg.Items.IndexOf(VCategoryText);
  end;
  if VIndex >= 0 then begin
    Result := ICategory(Pointer(CBKateg.Items.Objects[VIndex]));
  end;
  if Result = nil then begin
    VCategory := FCategoryDB.Factory.CreateNew(VCategoryText);
    Result := FCategoryDB.GetCategoryByName(VCategory.Name);
    if Result = nil then begin
      Result := FCategoryDB.UpdateCategory(nil, VCategory);
    end;
  end;
  if Result <> nil then begin
    FLastUsedCategoryName := Result.Name;
  end;
end;

procedure TfrMarkCategorySelectOrAdd.Init(const ACategory: ICategory);
var
  i: Integer;
  VCategory: ICategory;
begin
  FCategoryList := FCategoryDB.GetCategoriesList;
  CategoryListToStrings(FCategoryList, CBKateg.Items);
  CBKateg.Sorted := True;
  if ACategory <> nil then begin
    for i := 0 to CBKateg.Items.Count - 1 do begin
      VCategory := ICategory(Pointer(CBKateg.Items.Objects[i]));
      if VCategory <> nil then begin
        if VCategory.IsSame(ACategory) then begin
          CBKateg.ItemIndex := i;
          Break;
        end;
      end;
    end;
  end else begin
    VCategory := FCategoryDB.Factory.CreateNew(FLastUsedCategoryName);
    CBKateg.Text := VCategory.Name;
    CBKateg.ItemIndex := -1;
  end;
end;

end.
