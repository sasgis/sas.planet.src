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
  i_IMapViewGoto,
  i_GeoCoder,
  i_ISearchResultPresenter;

type
  TfrmSearchResults = class(TForm)
    lvResults: TListView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvResultsDblClick(Sender: TObject);
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
    procedure ShowSearchResults(ASearchResult: IGeoCodeResult);
  public
    constructor Create(AMapGoto: IMapViewGoto);
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
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
    FMapGoto.GotoPos(VPlacemark.GetPoint);
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
  ASearchResult: IGeoCodeResult);
var
  VEnum: IEnumUnknown;
  VPlacemark: IGeoCodePalcemark;
  i: Cardinal;
begin
  if ASearchResult.GetPlacemarksCount <= 0 then begin
    ShowMessage(SAS_STR_notfound);
  end else begin
    if ASearchResult.GetPlacemarksCount = 1 then begin
      VEnum := ASearchResult.GetPlacemarks;
      if VEnum.Next(1, VPlacemark, @i) = S_OK then begin
        FMapGoto.GotoPos(VPlacemark.GetPoint);
        ShowMessage(SAS_STR_foundplace+' "'+VPlacemark.GetAddress+'"');
      end;
    end else begin
      TfrmSearchResults.ShowSearchResults(FMapGoto, ASearchResult);
    end;
  end;
end;

end.
