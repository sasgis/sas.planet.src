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
  i_IMapViewGoto,
  i_GeoCoder,
  i_ISearchResultPresenter;

type
  TfrmSearchResults = class(TCommonFormParent)
    lvResults: TListView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvResultsDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvResultsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FMapGoto: IMapViewGoto;
    FSearchResult: IGeoCodeResult;
    procedure ShowStart;
  public
    class procedure ShowSearchResults(AMapGoto: IMapViewGoto; ASearchResult: IGeoCodeResult);
    destructor Destroy; override;
  end;

  TSearchResultPresenterWithForm = class(TInterfacedObject, ISearchResultPresenter)
  private
    FMapGoto: IMapViewGoto;
    procedure ShowSearchResults(ASearchResult: IGeoCodeResult; AZoom: Byte);
  public
    constructor Create(AMapGoto: IMapViewGoto);
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  u_GlobalState,
  UResStrings;

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

procedure TfrmSearchResults.lvResultsDblClick(Sender: TObject);
var
  VListItem: TListItem;
  VPlacemark: IGeoCodePalcemark;
begin
  VListItem := lvResults.Selected;
  if VListItem <> nil then begin
    VPlacemark := IGeoCodePalcemark(VListItem.Data);
    FMapGoto.GotoPos(VPlacemark.GetPoint, GState.ViewState.GetCurrentZoom);
  end;
end;

class procedure TfrmSearchResults.ShowSearchResults(
  AMapGoto: IMapViewGoto; ASearchResult: IGeoCodeResult);
var
  VForm: TfrmSearchResults;
begin
  VForm := TfrmSearchResults.Create(Application);
  VForm.FMapGoto := AMapGoto;
  VForm.FSearchResult := ASearchResult;
  VForm.ShowStart;
end;

procedure TfrmSearchResults.ShowStart;
var
  VListItem: TListItem;
  VEnum: IEnumUnknown;
  VPlacemark: IGeoCodePalcemark;
  i: Cardinal;
  VItemCount: Integer;
  VRect: TRect;
  VSize: TPoint;
  VNewClientHeight: Integer;
begin
  lvResults.Clear;
  VEnum := FSearchResult.GetPlacemarks;
  while VEnum.Next(1, VPlacemark, @i) = S_OK do begin
    VListItem := lvResults.Items.Add;
    VListItem.Data := Pointer(VPlacemark);
    VListItem.Caption := VPlacemark.GetAddress;
    VListItem.SubItems.Add(FloatToStr(VPlacemark.GetPoint.Y));
    VListItem.SubItems.Add(FloatToStr(VPlacemark.GetPoint.X));
  end;
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
  Show;
end;

{ TSearchResultPresenterWithForm }

constructor TSearchResultPresenterWithForm.Create(AMapGoto: IMapViewGoto);
begin
  FMapGoto := AMapGoto;
end;

destructor TSearchResultPresenterWithForm.Destroy;
begin
  FMapGoto := nil;
  inherited;
end;

procedure TSearchResultPresenterWithForm.ShowSearchResults(
  ASearchResult: IGeoCodeResult; AZoom: Byte);
var
  VEnum: IEnumUnknown;
  VPlacemark: IGeoCodePalcemark;
  i: Cardinal;
begin
  if ASearchResult.GetResultCode in [200, 203] then begin
    if ASearchResult.GetPlacemarksCount <= 0 then begin
      ShowMessage(SAS_STR_notfound);
    end else begin
      if ASearchResult.GetPlacemarksCount = 1 then begin
        VEnum := ASearchResult.GetPlacemarks;
        if VEnum.Next(1, VPlacemark, @i) = S_OK then begin
          FMapGoto.GotoPos(VPlacemark.GetPoint, AZoom);
          if ASearchResult.GetResultCode = 200 then begin
            ShowMessage(SAS_STR_foundplace+' "'+VPlacemark.GetAddress+'"');
          end;
        end;
      end else begin
        TfrmSearchResults.ShowSearchResults(FMapGoto, ASearchResult);
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
