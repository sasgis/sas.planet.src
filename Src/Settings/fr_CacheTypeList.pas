unit fr_CacheTypeList;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  i_LanguageManager,
  i_TileStorageTypeList,
  i_TileStorageTypeListItem,
  u_CommonFormAndFrameParents;

type
  TCacheTypeListOption = (
    foAllowAll,
    foAllowInMemory,
    foAllowFileSys,
    foAllowNoFileSys,
    foDisallowInMemory,
    foDisallowFileSys,
    foDisallowNoFileSys
  );

  TCacheTypeListOptions = set of TCacheTypeListOption;

  TfrCacheTypeList = class(TFrame)
    cbbCacheType: TComboBox;
    procedure cbbCacheTypeChange(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    FOptions: TCacheTypeListOptions;
    function IsItemAllowed(
      const AItem: ITileStorageTypeListItem
    ): Boolean; inline;
    procedure FillItems(
      const ATypesList: ITileStorageTypeListStatic;
      const AWithDefaultItem: Boolean
    );
  public
    procedure Show(AParent: TWinControl);
    function GetIntCode: Integer;
    procedure SetIntCode(const AValue: Integer);
    property IntCode: Integer read GetIntCode write SetIntCode;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const AWithDefaultItem: Boolean = False;
      const AFilterOptions: TCacheTypeListOptions = [foAllowAll];
      const AOnChange: TNotifyEvent = nil
    );
  end;

implementation

{$R *.dfm}

uses
  gnugettext,
  c_CacheTypeCodes,
  i_TileStorageAbilities;

{ TfrCacheTypeList }

constructor TfrCacheTypeList.Create(
  const ALanguageManager: ILanguageManager;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const AWithDefaultItem: Boolean;
  const AFilterOptions: TCacheTypeListOptions;
  const AOnChange: TNotifyEvent
);
begin
  inherited Create(ALanguageManager);
  FOnChange := AOnChange;

  FOptions := AFilterOptions;
  if
    not (foAllowAll in FOptions) and
    not (foAllowInMemory in FOptions) and
    not (foAllowFileSys in FOptions) and
    not (foAllowNoFileSys in FOptions) then
  begin
    if not (foDisallowInMemory in FOptions) then Include(FOptions, foAllowInMemory);
    if not (foDisallowFileSys in FOptions) then Include(FOptions, foAllowFileSys);
    if not (foDisallowNoFileSys in FOptions) then Include(FOptions, foAllowNoFileSys);
  end;

  FillItems(ATileStorageTypeList, AWithDefaultItem);
end;

procedure TfrCacheTypeList.Show(AParent: TWinControl);
begin
  Parent := AParent;
end;

function TfrCacheTypeList.IsItemAllowed(const AItem: ITileStorageTypeListItem): Boolean;
var
  VItemAbilities: ITileStorageTypeAbilities;
begin
  Result := False;

  if not AItem.CanUseAsDefault then begin
    Exit;
  end;

  if foAllowAll in FOptions then begin
    Result := True;
  end else begin
    VItemAbilities := AItem.StorageType.Abilities;
    case VItemAbilities.StorageClass of
      tstcInSeparateFiles: begin
        Result := (foAllowFileSys in FOptions) and not (foDisallowFileSys in FOptions);
      end;
      tstcInMemory: begin
        Result := (foAllowInMemory in FOptions) and not (foDisallowInMemory in FOptions);
      end;
      tstcOther: begin
        Result := (foAllowNoFileSys in FOptions) and not (foDisallowNoFileSys in FOptions);
      end;
    end;
  end;
end;

procedure TfrCacheTypeList.FillItems(
  const ATypesList: ITileStorageTypeListStatic;
  const AWithDefaultItem: Boolean
);
var
  I: Integer;
  VItem: ITileStorageTypeListItem;
begin
  Assert(Assigned(ATypesList));

  cbbCacheType.Clear;

  if AWithDefaultItem then begin
    cbbCacheType.Items.AddObject(_('By default'), TObject(c_File_Cache_Id_DEFAULT));
  end;

  for I := 0 to ATypesList.Count - 1 do begin
    VItem := ATypesList.Items[I];
    if IsItemAllowed(VItem) then begin
      cbbCacheType.Items.AddObject(VItem.Caption, TObject(VItem.IntCode));
    end;
  end;

  if cbbCacheType.Items.Count > 0 then begin
    cbbCacheType.ItemIndex := 0;
    cbbCacheType.DropDownCount := cbbCacheType.Items.Count;
  end;
end;

function TfrCacheTypeList.GetIntCode: Integer;
var
  I: Integer;
begin
  I := cbbCacheType.ItemIndex;
  Result := Integer(cbbCacheType.Items.Objects[I]);
end;

procedure TfrCacheTypeList.SetIntCode(const AValue: Integer);
var
  I: Integer;
  VCode: Integer;
  VFound: Boolean;
begin
  VFound := False;
  for I := 0 to cbbCacheType.Items.Count - 1 do begin
    VCode := Integer(cbbCacheType.Items.Objects[I]);
    if VCode = AValue then begin
      cbbCacheType.ItemIndex := I;
      VFound := True;
      Break;
    end;
  end;
  Assert(VFound);
  if Assigned(FOnChange) and VFound then begin
    FOnChange(Self);
  end;
end;

procedure TfrCacheTypeList.cbbCacheTypeChange(Sender: TObject);
begin
  if Assigned(FOnChange) then begin
    FOnChange(Self);
  end;
end;

end.
