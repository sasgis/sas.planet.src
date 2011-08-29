unit u_SearchResults;

interface

uses
  Dialogs,
  Controls,
  i_MapViewGoto,
  i_ViewPortState,
  i_ValueToStringConverter,
  i_GeoCoder,
  i_SearchResultPresenter,
  fr_SearchResultsItem;

type
  TSearchResultPresenterOnPanel = class(TInterfacedObject, ISearchResultPresenter)
  private
    FMapGoto: IMapViewGoto;
    FViewPortState: IViewPortState;
    FDrawParent:TWinControl;
    FSearchWindow:TWinControl;
    FValueConverterConfig: IValueToStringConverterConfig;
    FSearchItems:array of TfrSearchResultsItem;
  protected
    procedure ClearSearchResults;
    procedure ShowSearchResults(ASearchResult: IGeoCodeResult; AZoom: Byte);
  public
    constructor Create(
      AMapGoto: IMapViewGoto;
      ADrawParent: TWinControl;
      ASearchWindow: TWinControl;
      AValueConverterConfig: IValueToStringConverterConfig;
      AViewPortState: IViewPortState
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  u_ResStrings;

{ TSearchResultPresenterOnPanel }

constructor TSearchResultPresenterOnPanel.Create(
  AMapGoto: IMapViewGoto;
  ADrawParent: TWinControl;
  ASearchWindow: TWinControl;
  AValueConverterConfig: IValueToStringConverterConfig;
  AViewPortState: IViewPortState
);
begin
  FMapGoto := AMapGoto;
  FValueConverterConfig := AValueConverterConfig;
  FViewPortState := AViewPortState;
  FDrawParent := ADrawParent;
  FSearchWindow := ASearchWindow;
end;

destructor TSearchResultPresenterOnPanel.Destroy;
begin
  FMapGoto := nil;
  ClearSearchResults;
  inherited;
end;

procedure TSearchResultPresenterOnPanel.ClearSearchResults;
var i:integer;
begin
  for i := 0 to length(FSearchItems) - 1 do begin
    FSearchItems[i].Free;
  end;
  SetLength(FSearchItems,0);
end;

procedure TSearchResultPresenterOnPanel.ShowSearchResults(
  ASearchResult: IGeoCodeResult; AZoom: Byte);
var
  VPlacemark: IGeoCodePlacemark;
  VEnum: IEnumUnknown;
  i: Cardinal;
  LengthFSearchItems:integer;
  VItemForGoTo: IGeoCodePlacemark;
begin
  ClearSearchResults;
  VItemForGoTo := nil;
  VEnum := ASearchResult.GetPlacemarks;
  if ASearchResult.GetPlacemarksCount>1 then begin
    FSearchWindow.Show;
  end;
  while VEnum.Next(1, VPlacemark, @i) = S_OK do begin
    if VItemForGoTo = nil then begin
      VItemForGoTo := VPlacemark;
    end;
    LengthFSearchItems:=length(FSearchItems);
    SetLength(FSearchItems,LengthFSearchItems+1);
    FSearchItems[LengthFSearchItems]:=TfrSearchResultsItem.Create(nil, FDrawParent, VPlacemark, FMapGoto);
    if LengthFSearchItems>0 then begin
      FSearchItems[LengthFSearchItems].Top:=FSearchItems[LengthFSearchItems-1].Top+1
    end;
  end;
  if ASearchResult.GetResultCode in [200, 203] then begin
    if VItemForGoTo = nil then begin
      ShowMessage(SAS_STR_notfound);
    end else begin
      FMapGoto.GotoPos(VItemForGoTo.GetPoint, AZoom);
      if ASearchResult.GetPlacemarksCount = 1 then begin
        if ASearchResult.GetResultCode = 200 then begin
          ShowMessage(SAS_STR_foundplace+' "'+VItemForGoTo.GetAddress+'"');
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

end.
