unit fr_TilesGenPrev;

interface

uses
  Types,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  CheckLst,
  ExtCtrls,
  fr_MapSelect,
  t_CommonTypes,
  i_LanguageManager,
  i_ImageResamplerFactory,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_ImageResamplerConfig,
  i_VectorItemLonLat,
  i_RegionProcessParamsFrame,
  u_MapType,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameTilesGenPrev = interface(IRegionProcessParamsFrameBase)
    ['{34B156A8-D8DD-4EFF-AF55-70C93C3ADE17}']
    function GetIsReplace: Boolean;
    property IsReplace: Boolean read GetIsReplace;

    function GetIsSaveFullOnly: Boolean;
    property IsSaveFullOnly: Boolean read GetIsSaveFullOnly;

    function GetIsUseTilesFromPrevZoom: Boolean;
    property IsUseTilesFromPrevZoom: Boolean read GetIsUseTilesFromPrevZoom;

    function GetIsCreateAllFromFirstZoom: Boolean;
    property IsCreateAllFromFirstZoom: Boolean read GetIsCreateAllFromFirstZoom;

    function GetResampler: IImageResamplerFactory;
    property Resampler: IImageResamplerFactory read GetResampler;
  end;

type
  TfrTilesGenPrev = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTilesGenPrev
    )
    pnlBottom: TPanel;
    pnlRight: TPanel;
    pnlCenter: TPanel;
    lblStat: TLabel;
    pnlTop: TPanel;
    cbbFromZoom: TComboBox;
    lblFromZoom: TLabel;
    chkAllZooms: TCheckBox;
    lblZooms: TLabel;
    chklstZooms: TCheckListBox;
    cbbResampler: TComboBox;
    lblResampler: TLabel;
    chkReplace: TCheckBox;
    chkSaveFullOnly: TCheckBox;
    chkFromPrevZoom: TCheckBox;
    chkUsePrevTiles: TCheckBox;
    Bevel1: TBevel;
    pnlMapSelect: TPanel;
    pnlZoom: TPanel;
    pnlFrame: TPanel;
    lblMapCaption: TLabel;
    procedure cbbFromZoomChange(Sender: TObject);
    procedure chkAllZoomsClick(Sender: TObject);
    procedure chkFromPrevZoomClick(Sender: TObject);
    procedure chklstZoomsClickCheck(Sender: TObject);
    procedure chkReplaceClick(Sender: TObject);
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FImageResamplerConfig: IImageResamplerConfig;
    FfrMapSelect: TfrMapSelect;
    procedure InitResamplersList(const AList: IImageResamplerFactoryList; ABox: TComboBox);
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: ILonLatPolygon
    );
  private
    function GetMapType: TMapType;
    function GetZoomArray: TByteDynArray;
    function GetAllowGenPrev(AMapType: TMapType): boolean;
    function GetIsReplace: Boolean;
    function GetIsSaveFullOnly: Boolean;
    function GetIsUseTilesFromPrevZoom: Boolean;
    function GetIsCreateAllFromFirstZoom: Boolean;
    function GetResampler: IImageResamplerFactory;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AImageResamplerConfig: IImageResamplerConfig
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  i_GUIDListStatic;

{$R *.dfm}

const
  CZommDeltaMax = 8;

constructor TfrTilesGenPrev.Create(
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AImageResamplerConfig: IImageResamplerConfig
);
begin
  TP_Ignore(Self, 'cbbResampler.Items');
  TP_Ignore(Self, 'cbbResampler.Text');
  inherited Create(ALanguageManager);
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
  FImageResamplerConfig := AImageResamplerConfig;
  FfrMapSelect :=
    TfrMapSelect.Create(
      ALanguageManager,
      AMainMapsConfig,
      AGUIConfigList,
      AFullMapsSet,
      mfAll, // show maps and layers
      False,  // add -NO- to combobox
      False,  // show disabled map
      GetAllowGenPrev
    );
end;

destructor TfrTilesGenPrev.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  inherited;
end;

function TfrTilesGenPrev.GetAllowGenPrev(AMapType: TMapType): boolean;
begin
  Result := (AMapType.IsBitmapTiles) and (AMapType.TileStorage.State.GetStatic.WriteAccess <> asDisabled);
end;

procedure TfrTilesGenPrev.cbbFromZoomChange(Sender: TObject);
var
  i: integer;
begin
  chklstZooms.Items.Clear;
  for i := cbbFromZoom.ItemIndex + 1 downto 1 do begin
    chklstZooms.Items.Add(inttostr(i));
  end;
  chklstZoomsClickCheck(nil);
  chklstZooms.Repaint;
end;

procedure TfrTilesGenPrev.chkAllZoomsClick(Sender: TObject);
var
  i: integer;
begin
  if chkAllZooms.State <> cbGrayed then begin
    for i := 0 to chklstZooms.Count - 1 do begin
      if chklstZooms.ItemEnabled[i] or chkFromPrevZoom.Checked then begin
        if chkFromPrevZoom.Checked then chklstZooms.ItemEnabled[i] := true;
        chklstZooms.Checked[i] := chkAllZooms.Checked;
      end;
    end;
  end;
end;

procedure TfrTilesGenPrev.chkFromPrevZoomClick(Sender: TObject);
begin
  chklstZoomsClickCheck(nil);
end;

procedure TfrTilesGenPrev.chklstZoomsClickCheck(Sender: TObject);
var
  i: Integer;
  VLastCheckedZoom: Integer;
  VZoom: Integer;
  VSourceZoom: Integer;
  VAllChecked: Boolean;
  VAllUnChecked: Boolean;
begin
  if chkFromPrevZoom.Checked then begin
    VSourceZoom := cbbFromZoom.ItemIndex + 1;
    VLastCheckedZoom := VSourceZoom;
    i := 0;
    while i < chklstZooms.Items.Count do begin
      VZoom := VSourceZoom - i - 1;
      if VLastCheckedZoom - VZoom > CZommDeltaMax then begin
        Break;
      end else begin
        chklstZooms.ItemEnabled[i] := True;
        if chklstZooms.Checked[i] then begin
          VLastCheckedZoom := VZoom;
        end;
      end;
      Inc(i);
    end;
    while i < chklstZooms.Items.Count do begin
      chklstZooms.ItemEnabled[i] := False;
      Inc(i);
    end;
  end else begin
    for i := CZommDeltaMax to chklstZooms.Items.Count - 1 do begin
      chklstZooms.ItemEnabled[i] := false;
    end;
  end;
  VAllChecked := True;
  VAllUnChecked := True;
  for i := 0 to chklstZooms.Items.Count - 1 do begin
    if chklstZooms.ItemEnabled[i] then begin
      if chklstZooms.Checked[i] then begin
        VAllUnChecked := False;
      end else begin
        VAllChecked := False;
      end;
    end;
  end;
  if VAllChecked then begin
    chkAllZooms.State := cbChecked;
  end else if VAllUnChecked then begin
    chkAllZooms.State := cbUnchecked;
  end else begin
    chkAllZooms.State := cbGrayed;
  end;
end;

procedure TfrTilesGenPrev.chkReplaceClick(Sender: TObject);
begin
 chkUsePrevTiles.Enabled := chkReplace.Checked;
end;

function TfrTilesGenPrev.GetIsCreateAllFromFirstZoom: Boolean;
begin
  Result := not chkFromPrevZoom.Checked;
end;

function TfrTilesGenPrev.GetIsReplace: Boolean;
begin
  Result := chkReplace.Checked;
end;

function TfrTilesGenPrev.GetIsSaveFullOnly: Boolean;
begin
  Result := chkSaveFullOnly.Checked;
end;

function TfrTilesGenPrev.GetIsUseTilesFromPrevZoom: Boolean;
begin
  Result := chkUsePrevTiles.Checked;
end;

function TfrTilesGenPrev.GetMapType: TMapType;
begin
  Result := FfrMapSelect.GetSelectedMapType;
end;

function TfrTilesGenPrev.GetResampler: IImageResamplerFactory;
begin
  try
    if cbbResampler.ItemIndex >= 0 then begin
      Result := FImageResamplerConfig.GetList.Items[cbbResampler.ItemIndex];
    end else begin
      Result := FImageResamplerConfig.GetActiveFactory;
    end;
  except
    Result := FImageResamplerConfig.GetActiveFactory;
  end;
end;

function TfrTilesGenPrev.GetZoomArray: TByteDynArray;
var
  i: Integer;
  VCount: Integer;
  VSourceZoom: Byte;
begin
  Result := nil;
  VCount := 1;
  SetLength(Result, VCount);
  VSourceZoom := cbbFromZoom.ItemIndex + 1;
  Result[0] := VSourceZoom;
  if VSourceZoom > 0 then begin
    for i := 0 to VSourceZoom  - 1 do begin
      if chklstZooms.ItemEnabled[i] then begin
        if chklstZooms.Checked[i] then begin
          SetLength(Result, VCount + 1);
          Result[VCount] := VSourceZoom - 1 - i;
          Inc(VCount);
        end;
      end;
    end;
  end;
end;

procedure TfrTilesGenPrev.Init(
  const AZoom: byte;
  const APolygon: ILonLatPolygon
  );
var
  i: integer;
begin
  cbbFromZoom.Items.Clear;
  for i := 2 to 24 do begin
    cbbFromZoom.Items.Add(inttostr(i));
  end;
  if AZoom > 0 then begin
    cbbFromZoom.ItemIndex := AZoom - 1;
  end else begin
    cbbFromZoom.ItemIndex := 0;
  end;
  cbbFromZoomChange(cbbFromZoom);
  InitResamplersList(FImageResamplerConfig.GetList, cbbResampler);
  cbbResampler.ItemIndex := FImageResamplerConfig.ActiveIndex;
  FfrMapSelect.Show(pnlFrame);
end;

procedure TfrTilesGenPrev.InitResamplersList(
  const AList: IImageResamplerFactoryList;
  ABox: TComboBox
  );
var
  i: Integer;
begin
  ABox.Items.Clear;
  for i := 0 to AList.Count - 1 do begin
    ABox.Items.Add(AList.Captions[i]);
  end;
end;

end.
