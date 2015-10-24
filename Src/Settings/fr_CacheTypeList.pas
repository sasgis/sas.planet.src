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
  i_TileStorageAbilities,
  i_TileStorageTypeList,
  i_TileStorageTypeListItem,
  u_CommonFormAndFrameParents;

type
  TTileStorageTypeClassSet = set of TTileStorageTypeClass;

const
  CTileStorageTypeClassAll = [tstcInSeparateFiles, tstcInMemory, tstcOther];

type
  TTileStorageAbilitiesClass = (tsacRead, tsacScan, tsacAdd, tsacDelete, tsacReplace);
  TTileStorageAbilitiesClassSet = set of TTileStorageAbilitiesClass;

type
  TfrCacheTypeList = class(TFrame)
    cbbCacheType: TComboBox;
    procedure cbbCacheTypeChange(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    FOptions: TTileStorageTypeClassSet;
    FRequaredAbilities: TTileStorageAbilitiesClassSet;
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
      const AFilterOptions: TTileStorageTypeClassSet = CTileStorageTypeClassAll;
      const ARequaredAbilities: TTileStorageAbilitiesClassSet = [];
      const AOnChange: TNotifyEvent = nil
    );
  end;

implementation

{$R *.dfm}

uses
  gnugettext,
  c_CacheTypeCodes;

{ TfrCacheTypeList }

constructor TfrCacheTypeList.Create(
  const ALanguageManager: ILanguageManager;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const AWithDefaultItem: Boolean;
  const AFilterOptions: TTileStorageTypeClassSet;
  const ARequaredAbilities: TTileStorageAbilitiesClassSet;
  const AOnChange: TNotifyEvent
);
begin
  Assert(AFilterOptions <> []);
  inherited Create(ALanguageManager);
  FOnChange := AOnChange;
  FOptions := AFilterOptions;
  FRequaredAbilities := ARequaredAbilities;
  
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

  VItemAbilities := AItem.StorageType.Abilities;
  case VItemAbilities.StorageClass of
    tstcInSeparateFiles: begin
      Result := (tstcInSeparateFiles in FOptions);
    end;
    tstcInMemory: begin
      Result := (tstcInMemory in FOptions);
    end;
    tstcOther: begin
      Result := (tstcOther in FOptions);
    end;
  end;
  if Result then begin
    if tsacRead in FRequaredAbilities then begin
      if not VItemAbilities.BaseStorageAbilities.AllowRead then begin
        Result := False;
        Exit;
      end;
    end;
    if tsacScan in FRequaredAbilities then begin
      if not VItemAbilities.BaseStorageAbilities.AllowScan then begin
        Result := False;
        Exit;
      end;
    end;
    if tsacAdd in FRequaredAbilities then begin
      if not VItemAbilities.BaseStorageAbilities.AllowAdd then begin
        Result := False;
        Exit;
      end;
    end;
    if tsacDelete in FRequaredAbilities then begin
      if not VItemAbilities.BaseStorageAbilities.AllowDelete then begin
        Result := False;
        Exit;
      end;
    end;
    if tsacReplace in FRequaredAbilities then begin
      if not VItemAbilities.BaseStorageAbilities.AllowReplace then begin
        Result := False;
        Exit;
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
