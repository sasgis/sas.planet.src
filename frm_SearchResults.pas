unit frm_SearchResults;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  ComCtrls,
  Forms,
  Dialogs,
  u_CommonFormAndFrameParents,
  t_GeoTypes,
  i_Datum,
  i_MapViewGoto,
  i_ViewPortState,
  i_ValueToStringConverter,
  i_LocalCoordConverter,
  i_GeoCoder,
  i_SearchResultPresenter;

type
  TfrmSearchResults = class(TCommonFormParent)
    lvResults: TListView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvResultsDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvResultsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvResultsCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
  private
    FMapGoto: IMapViewGoto;
    FViewPortState: IViewPortState;
    FCurrLonLat: TDoublePoint;
    FCurrDatum: IDatum;
    FSearchResult: IGeoCodeResult;
    FValueConverterConfig: IValueToStringConverterConfig;
    procedure ShowStart;
    procedure AddListItems;
  public
    class procedure ShowSearchResults(
      AMapGoto: IMapViewGoto;
      AValueConverterConfig: IValueToStringConverterConfig;
      AViewPortState: IViewPortState;
      ASearchResult: IGeoCodeResult
    );
    destructor Destroy; override;
  end;

  TSearchResultPresenterWithForm = class(TInterfacedObject, ISearchResultPresenter)
  private
    FMapGoto: IMapViewGoto;
    FViewPortState: IViewPortState;
    FValueConverterConfig: IValueToStringConverterConfig;
    function GetNearestResult(ASearchResult: IGeoCodeResult): IGeoCodePlacemark;
  protected
    procedure ShowSearchResults(ASearchResult: IGeoCodeResult; AZoom: Byte);
  public
    constructor Create(
      AMapGoto: IMapViewGoto;
      AValueConverterConfig: IValueToStringConverterConfig;
      AViewPortState: IViewPortState
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  u_GlobalState,
  u_ResStrings;

{$R *.dfm}

destructor TfrmSearchResults.Destroy;
begin
  FMapGoto := nil;
  FSearchResult := nil;
  inherited;
end;

procedure TfrmSearchResults.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Action = caHide then begin
    Action := caFree;
  end;
end;

procedure TfrmSearchResults.lvResultsCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  VPlacemark: IGeoCodePlacemark;
  VDist1: Double;
  VDist2: Double;
  VDatum: IDatum;
  VCurrLonLat: TDoublePoint;
begin
  Compare := 0;
  VDatum := FCurrDatum;
  VCurrLonLat := FCurrLonLat;
  if VDatum <> nil then begin
    VPlacemark := IGeoCodePlacemark(Item1.Data);
    VDist1 := VDatum.CalcDist(VCurrLonLat, VPlacemark.GetPoint);
    VPlacemark := IGeoCodePlacemark(Item2.Data);
    VDist2 := VDatum.CalcDist(VCurrLonLat, VPlacemark.GetPoint);
    if VDist1 < VDist2 then begin
      Compare := -1;
    end else begin
      Compare := 1;
    end;
  end;
end;

procedure TfrmSearchResults.lvResultsDblClick(Sender: TObject);
var
  VListItem: TListItem;
  VPlacemark: IGeoCodePlacemark;
begin
  VListItem := lvResults.Selected;
  if VListItem <> nil then begin
    VPlacemark := IGeoCodePlacemark(VListItem.Data);
    FMapGoto.GotoPos(VPlacemark.GetPoint, GState.MainFormConfig.ViewPortState.GetCurrentZoom);
  end;
end;

class procedure TfrmSearchResults.ShowSearchResults(
  AMapGoto: IMapViewGoto;
  AValueConverterConfig: IValueToStringConverterConfig;
  AViewPortState: IViewPortState;
  ASearchResult: IGeoCodeResult
);
var
  VForm: TfrmSearchResults;
begin
  VForm := TfrmSearchResults.Create(Application);
  VForm.FMapGoto := AMapGoto;
  VForm.FSearchResult := ASearchResult;
  VForm.FValueConverterConfig := AValueConverterConfig;
  VForm.FViewPortState := AViewPortState;
  VForm.ShowStart;
end;

procedure TfrmSearchResults.AddListItems;
var
  VListItem: TListItem;
  VEnum: IEnumUnknown;
  VPlacemark: IGeoCodePlacemark;
  i: Cardinal;
  VDist: Double;
  VValueConverter: IValueToStringConverter;
begin
  lvResults.Clear;
  VValueConverter := FValueConverterConfig.GetStaticConverter;
  VEnum := FSearchResult.GetPlacemarks;
  while VEnum.Next(1, VPlacemark, @i) = S_OK do begin
    VListItem := lvResults.Items.Add;
    VListItem.Data := Pointer(VPlacemark);
    VListItem.Caption := VPlacemark.GetAddress;
    VDist := FCurrDatum.CalcDist(FCurrLonLat, VPlacemark.GetPoint);
    VListItem.SubItems.Add(VValueConverter.DistConvert(VDist));
    VListItem.SubItems.Add(VValueConverter.LonLatConvert(VPlacemark.GetPoint));
  end;
end;

procedure TfrmSearchResults.ShowStart;
var
  VListItem: TListItem;
  VItemCount: Integer;
  VRect: TRect;
  VSize: TPoint;
  VNewClientHeight: Integer;
  VLocalCoord: ILocalCoordConverter;
begin
  VLocalCoord := FViewPortState.GetVisualCoordConverter;
  FCurrLonLat := VLocalCoord.GetCenterLonLat;
  FCurrDatum := VLocalCoord.GetGeoConverter.Datum;

  AddListItems;
  VItemCount := lvResults.Items.Count;
  if VItemCount > 0 then begin
    lvResults.Items[0].Selected := True;
    VListItem := lvResults.Items[VItemCount - 1];
    VRect := VListItem.DisplayRect(drBounds);
    VSize := lvResults.ClientToParent(VRect.BottomRight);
    VNewClientHeight := VSize.Y + VRect.Bottom - VRect.Top;
    if VNewClientHeight < ClientHeight then begin
      ClientHeight := VNewClientHeight;
    end;
  end;
  lvResults.AlphaSort;
  Show;
end;

{ TSearchResultPresenterWithForm }

constructor TSearchResultPresenterWithForm.Create(
  AMapGoto: IMapViewGoto;
  AValueConverterConfig: IValueToStringConverterConfig;
  AViewPortState: IViewPortState
);
begin
  FMapGoto := AMapGoto;
  FValueConverterConfig := AValueConverterConfig;
  FViewPortState := AViewPortState;
end;

destructor TSearchResultPresenterWithForm.Destroy;
begin
  FMapGoto := nil;
  inherited;
end;

function TSearchResultPresenterWithForm.GetNearestResult(
  ASearchResult: IGeoCodeResult): IGeoCodePlacemark;
var
  VEnum: IEnumUnknown;
  VPlacemark: IGeoCodePlacemark;
  i: Cardinal;
  VLocalCoord: ILocalCoordConverter;
  VCurrLonLat: TDoublePoint;
  VDatum: IDatum;
  VMinDist: Double;
  VDist: Double;
begin
  VLocalCoord := FViewPortState.GetVisualCoordConverter;
  VCurrLonLat := VLocalCoord.GetCenterLonLat;
  VDatum := VLocalCoord.GetGeoConverter.Datum;

  VEnum := ASearchResult.GetPlacemarks;
  if VEnum.Next(1, VPlacemark, @i) = S_OK then begin
    Result := VPlacemark;
    VMinDist := VDatum.CalcDist(VCurrLonLat, VPlacemark.GetPoint);
    while VEnum.Next(1, VPlacemark, @i) = S_OK do begin
      VDist := VDatum.CalcDist(VCurrLonLat, VPlacemark.GetPoint);
      if VMinDist > VDist then begin
        VMinDist := VDist;
        Result := VPlacemark;
      end;
    end;
  end;
end;

procedure TSearchResultPresenterWithForm.ShowSearchResults(
  ASearchResult: IGeoCodeResult; AZoom: Byte);
var
  VPlacemark: IGeoCodePlacemark;
begin
  if ASearchResult.GetResultCode in [200, 203] then begin
    if ASearchResult.GetPlacemarksCount <= 0 then begin
      ShowMessage(SAS_STR_notfound);
    end else begin
      if ASearchResult.GetPlacemarksCount = 1 then begin
        VPlacemark := GetNearestResult(ASearchResult);
        if VPlacemark <> nil then begin
          FMapGoto.GotoPos(VPlacemark.GetPoint, AZoom);
          if ASearchResult.GetResultCode = 200 then begin
            ShowMessage(SAS_STR_foundplace+' "'+VPlacemark.GetAddress+'"');
          end;
        end;
      end else begin
        TfrmSearchResults.ShowSearchResults(FMapGoto, FValueConverterConfig, FViewPortState, ASearchResult);
        VPlacemark := GetNearestResult(ASearchResult);
        if VPlacemark <> nil then begin
          FMapGoto.GotoPos(VPlacemark.GetPoint, AZoom);
        end;
      end;
    end;
  end else begin
    case ASearchResult.GetResultCode of
      503: begin
        ShowMessage(SAS_ERR_Noconnectionstointernet + #13#10 + ASearchResult.GetMessage);
      end;
      407: begin
        ShowMessage(SAS_ERR_Authorization + #13#10 + ASearchResult.GetMessage);
      end;
      416: begin
        ShowMessage('Ошибка разбора ответа: '+ #13#10 + ASearchResult.GetMessage);
      end;
      404: begin
        ShowMessage(SAS_STR_notfound);
      end;
      else begin
        ShowMessage('Неизвестная ошибка: '+ #13#10 + ASearchResult.GetMessage);
      end;
    end
  end;
end;

procedure TfrmSearchResults.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    close;
  end;
end;

procedure TfrmSearchResults.lvResultsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    close;
  end else if Key = VK_RETURN then begin
    lvResultsDblClick(lvResults);
  end;
end;

end.
