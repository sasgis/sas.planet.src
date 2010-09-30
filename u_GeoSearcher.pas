unit u_GeoSearcher;

interface

uses
  t_GeoTypes,
  i_ISearchResultPresenter,
  i_GeoCoder;

type
  TGeoSearcher = class
  private
    FGeoCoder: IGeoCoder;
    FPresenter: ISearchResultPresenter;
  public
    constructor Create(AGeoCoder: IGeoCoder; APresenter: ISearchResultPresenter);
    destructor Destroy; override;
    procedure ModalSearch(ASearchText: String; ACurrentPos: TExtendedPoint);
  end;

implementation

uses
  Dialogs,
  SysUtils,
  UResStrings;

{ TGeoSearcher }

constructor TGeoSearcher.Create(AGeoCoder: IGeoCoder;
  APresenter: ISearchResultPresenter);
begin
  FGeoCoder := AGeoCoder;
  FPresenter := APresenter;
end;

destructor TGeoSearcher.Destroy;
begin
  FGeoCoder := nil;
  FPresenter := nil;
  inherited;
end;

procedure TGeoSearcher.ModalSearch(ASearchText: String; ACurrentPos: TExtendedPoint);
var
  VText: String;
  VResult: IGeoCodeResult;
begin
  VText := Trim(ASearchText);
  if VText <> '' then begin
    VResult := FGeoCoder.GetLocations(VText, ACurrentPos);
    if VResult = nil then begin
      ShowMessage('Ошибка получения результатов поиска');
    end else begin
      if VResult.GetResultCode = 200 then begin
        FPresenter.ShowSearchResults(VResult, 0);
      end else begin
        case VResult.GetResultCode of
          503: begin
            ShowMessage(SAS_ERR_Noconnectionstointernet + #13#10 + VResult.GetMessage);
          end;
          407: begin
            ShowMessage(SAS_ERR_Authorization + #13#10 + VResult.GetMessage);
          end;
          416: begin
            ShowMessage('Ошибка разбора ответа: '+ #13#10 + VResult.GetMessage);
          end;
          404: begin
            ShowMessage(SAS_STR_notfound);
          end;
          else begin
            ShowMessage('Неизвестная ошибка: '+ #13#10 + VResult.GetMessage);
          end;
        end
      end;
    end;
  end;
end;

end.
