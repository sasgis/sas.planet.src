{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_SearchResultPresenterOnPanel;

interface

uses
  Classes,
  Controls,
  Menus,
  i_MapViewGoto,
  i_CoordToStringConverter,
  i_LastSearchResult,
  i_GeoCoder,
  i_InternalBrowser,
  i_SearchResultPresenter,
  u_BaseInterfacedObject,
  fr_SearchResultsItem;

type
  TSearchResultPresenterOnPanel = class(TBaseInterfacedObject, ISearchResultPresenter)
  private
    FMapGoto: IMapViewGoto;
    FIntrnalBrowser: IInternalBrowser;
    FDrawParent: TWinControl;
    FPopUp: TPopupMenu;
    FCoordToStringConverter: ICoordToStringConverterChangeable;
    FLastSearchResults: ILastSearchResult;
    FOnShowResults: TNotifyEvent;
    FSearchItems: array of TfrSearchResultsItem;
  private
    procedure ClearSearchResults;
    procedure ShowSearchResults(
      const ASearchResult: IGeoCodeResult
    );
  public
    constructor Create(
      const AIntrnalBrowser: IInternalBrowser;
      const AMapGoto: IMapViewGoto;
      ADrawParent: TWinControl;
      APopUp: TPopupMenu;
      AOnShowResults: TNotifyEvent;
      const ACoordToStringConverter: ICoordToStringConverterChangeable;
      const ALastSearchResults: ILastSearchResult
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  SysUtils,
  gnugettext,
  i_VectorDataItemSimple,
  i_GeometryLonLat,
  u_Dialogs,
  u_ResStrings;

{ TSearchResultPresenterOnPanel }

constructor TSearchResultPresenterOnPanel.Create(
  const AIntrnalBrowser: IInternalBrowser;
  const AMapGoto: IMapViewGoto;
  ADrawParent: TWinControl;
  APopUp: TPopupMenu;
  AOnShowResults: TNotifyEvent;
  const ACoordToStringConverter: ICoordToStringConverterChangeable;
  const ALastSearchResults: ILastSearchResult
);
begin
  inherited Create;
  FPopUp := APopUp;
  FIntrnalBrowser := AIntrnalBrowser;
  FMapGoto := AMapGoto;
  FCoordToStringConverter := ACoordToStringConverter;
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
  I: Integer;
begin
  for I := 0 to Length(FSearchItems) - 1 do begin
    FSearchItems[I].Free;
  end;
  SetLength(FSearchItems, 0);
end;

procedure TSearchResultPresenterOnPanel.ShowSearchResults(
  const ASearchResult: IGeoCodeResult
);
var
  I: Cardinal;
  VPlacemark: IVectorDataItem;
  VEnum: IEnumUnknown;
  VLen: Integer;
  VItemForGoTo: IVectorDataItem;
  VCount: Integer;
begin
  ClearSearchResults;
  VItemForGoTo := nil;

  FLastSearchResults.ClearGeoCodeResult;
  if ASearchResult.Count > 0 then begin
    FLastSearchResults.GeoCodeResult := ASearchResult;
    if ASearchResult.Count > 1 then begin
      FOnShowResults(Self);
    end;

    VCount := 0;
    VEnum := ASearchResult.GetEnum;
    while VEnum.Next(1, VPlacemark, @I) = S_OK do begin
      if VItemForGoTo = nil then begin
        VItemForGoTo := VPlacemark;
      end;
      VLen := Length(FSearchItems);
      SetLength(FSearchItems, VLen + 1);
      FSearchItems[VLen] :=
        TfrSearchResultsItem.Create(
          nil,
          FDrawParent,
          FPopUp,
          VPlacemark,
          FIntrnalBrowser,
          FMapGoto,
          FCoordToStringConverter
        );
      if VLen > 0 then begin
        FSearchItems[VLen].Top := FSearchItems[VLen - 1].Top + 1;
      end;
      Inc(VCount);
      if VCount > 100 then begin
        Break;
      end;
    end;
  end;

  if ASearchResult.GetResultCode in [200, 203] then begin
    if VItemForGoTo = nil then begin
      ShowInfoMessage(SAS_STR_notfound);
    end else begin
      FMapGoto.FitRectToScreen(VItemForGoTo.Geometry.Bounds.Rect);
      FMapGoto.ShowMarker(VItemForGoTo.Geometry.GetGoToPoint);
    end;
  end else begin
    case ASearchResult.GetResultCode of
      CGeoCodeExceptionResultCode: begin
        ShowErrorMessage(ASearchResult.GetMessage);
      end;
      CGeoCodeDownloadErrorResultCode: begin
        ShowErrorMessage(ASearchResult.GetMessage);
      end;
      CGeoCodeNoInternetConnectionResultCode: begin
        ShowErrorMessage(SAS_ERR_Noconnectionstointernet + #13#10 + ASearchResult.GetMessage);
      end;
      CGeoCodeNotFoundResultCode: begin
        ShowErrorMessage(SAS_STR_notfound);
      end;
      407: begin
        ShowErrorMessage(SAS_ERR_Authorization + #13#10 + ASearchResult.GetMessage);
      end;
      404: begin
        ShowErrorMessage(SAS_STR_notfound);
      end;
    else
      ShowErrorMessage(ASearchResult.GetMessage);
    end;
  end;
end;

end.
