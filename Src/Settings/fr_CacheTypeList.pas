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
  u_CommonFormAndFrameParents;

type
  TfrCacheTypeList = class(TCommonFrameParent)
    cbbCacheType: TComboBox;
  private
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
      const AWithDefaultItem: Boolean = False
    );
  end;

implementation

{$R *.dfm}

uses
  gnugettext,
  c_CacheTypeCodes,
  i_TileStorageTypeListItem;

{ TfrCacheTypeList }

constructor TfrCacheTypeList.Create(
  const ALanguageManager: ILanguageManager;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const AWithDefaultItem: Boolean
);
begin
  inherited Create(ALanguageManager);
  FillItems(ATileStorageTypeList, AWithDefaultItem);
end;

procedure TfrCacheTypeList.Show(AParent: TWinControl);
begin
  Parent := AParent;
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
    if VItem.CanUseAsDefault then begin
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
end;

end.
