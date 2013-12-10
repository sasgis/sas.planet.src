{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_SearchResults;

interface

uses
  Classes,
  Dialogs,
  Controls,
  Menus,
  i_MapViewGoto,
  i_LocalCoordConverterChangeable,
  i_ValueToStringConverter,
  i_LastSearchResultConfig,
  i_GeoCoder,
  i_InternalBrowser,
  i_SearchResultPresenter,
  u_BaseInterfacedObject,
  fr_SearchResultsItem;

type
  TSearchResultPresenterOnPanel = class(TBaseInterfacedObject, ISearchResultPresenter)
  private
    FMapGoto: IMapViewGoto;
    FViewPortState: ILocalCoordConverterChangeable;
    FIntrnalBrowser: IInternalBrowser;
    FDrawParent: TWinControl;
    FPopUp: TPopupMenu;
    FValueConverterConfig: IValueToStringConverterConfig;
    FLastSearchResults: ILastSearchResultConfig;
    FOnShowResults: TNotifyEvent;
    FSearchItems: array of TfrSearchResultsItem;
  private
    procedure ClearSearchResults;
    procedure ShowSearchResults(
      const ASearchResult: IGeoCodeResult;
      AZoom: Byte
    );
  public
    constructor Create(
      const AIntrnalBrowser: IInternalBrowser;
      const AMapGoto: IMapViewGoto;
      ADrawParent: TWinControl;
      APopUp: TPopupMenu;
      AOnShowResults: TNotifyEvent;
      const AValueConverterConfig: IValueToStringConverterConfig;
      const ALastSearchResults: ILastSearchResultConfig;
      const AViewPortState: ILocalCoordConverterChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  u_ResStrings;

{ TSearchResultPresenterOnPanel }

constructor TSearchResultPresenterOnPanel.Create(
  const AIntrnalBrowser: IInternalBrowser;
  const AMapGoto: IMapViewGoto;
  ADrawParent: TWinControl;
  APopUp: TPopupMenu;
  AOnShowResults: TNotifyEvent;
  const AValueConverterConfig: IValueToStringConverterConfig;
  const ALastSearchResults: ILastSearchResultConfig;
  const AViewPortState: ILocalCoordConverterChangeable
);
begin
  inherited Create;
  FPopUp := APopUp;
  FIntrnalBrowser := AIntrnalBrowser;
  FMapGoto := AMapGoto;
  FValueConverterConfig := AValueConverterConfig;
  FViewPortState := AViewPortState;
  FDrawParent := ADrawParent;
  FLastSearchResults := ALastSearchResults;
  FOnShowResults := AOnShowResults;
end;

destructor TSearchResultPresenterOnPanel.Destroy;
begin
  FMapGoto := nil;
  ClearSearchResults;
  inherited;
end;

procedure TSearchResultPresenterOnPanel.ClearSearchResults;
var
  i: integer;
begin
  for i := 0 to length(FSearchItems) - 1 do begin
    FSearchItems[i].Free;
  end;
  SetLength(FSearchItems, 0);
end;

procedure TSearchResultPresenterOnPanel.ShowSearchResults(
  const ASearchResult: IGeoCodeResult;
  AZoom: Byte
);
var
  VPlacemark: IGeoCodePlacemark;
  VEnum: IEnumUnknown;
  i: Cardinal;
  LengthFSearchItems: integer;
  VItemForGoTo: IGeoCodePlacemark;
  VCnt: Integer;
begin
  ClearSearchResults;
  VItemForGoTo := nil;

  FLastSearchResults.ClearGeoCodeResult;
  if ASearchResult.GetPlacemarksCount > 0 then begin
    if ASearchResult.GetPlacemarksCount > 1 then begin
      FOnShowResults(Self);
      FLastSearchResults.GeoCodeResult := ASearchResult;
    end;

    VCnt := 0;
    VEnum := ASearchResult.GetPlacemarks;
    while VEnum.Next(1, VPlacemark, @i) = S_OK do begin
      if VItemForGoTo = nil then begin
        VItemForGoTo := VPlacemark;
      end;
      LengthFSearchItems := length(FSearchItems);
      SetLength(FSearchItems, LengthFSearchItems + 1);
      FSearchItems[LengthFSearchItems] :=
        TfrSearchResultsItem.Create(
          nil,
          FDrawParent,
          FPopUp,
          VPlacemark,
          FViewPortState,
          FIntrnalBrowser,
          FMapGoto
        );
      if LengthFSearchItems > 0 then begin
        FSearchItems[LengthFSearchItems].Top := FSearchItems[LengthFSearchItems - 1].Top + 1;
      end;
      Inc(VCnt);
      if VCnt > 100 then begin
        Break;
      end;
    end;
  end;

  if ASearchResult.GetResultCode in [200, 203] then begin
    if VItemForGoTo = nil then begin
      ShowMessage(SAS_STR_notfound);
    end else begin
      FMapGoto.GotoPos(VItemForGoTo.GetPoint.Point, AZoom, True);
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
        ShowMessage('Ошибка разбора ответа: ' + #13#10 + ASearchResult.GetMessage);
      end;
      404: begin
        ShowMessage(SAS_STR_notfound);
      end;
    else begin
      ShowMessage('Неизвестная ошибка: ' + #13#10 + ASearchResult.GetMessage);
    end;
    end;
  end;
end;

end.
