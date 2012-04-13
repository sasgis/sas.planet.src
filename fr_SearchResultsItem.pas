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

unit fr_SearchResultsItem;

interface

uses
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  i_GeoCoder,
  i_ViewPortState,
  i_MapViewGoto,
  i_InternalBrowser,
  Classes;

type
  TfrSearchResultsItem = class(TFrame)
    PanelCaption: TPanel;
    PanelFullDesc: TPanel;
    PanelDesc: TPanel;
    LabelDesc: TLabel;
    LabelFullDesc: TLabel;
    Bevel1: TBevel;
    LabelCaption: TLabel;
    procedure LabelFullDescMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelCaptionClick(Sender: TObject);
    procedure LabelDescDblClick(Sender: TObject);
  private
    FPlacemark: IGeoCodePlacemark;
    FViewPortState: IViewPortState;
    FMapGoto: IMapViewGoto;
    FIntrnalBrowser: IInternalBrowser;
  public
    constructor Create(
      AOwner: TComponent;
      AParent:TWinControl;
      APlacemark: IGeoCodePlacemark;
      AViewPortState: IViewPortState;
      AIntrnalBrowser: IInternalBrowser;
      AMapGoto: IMapViewGoto
    ); reintroduce;
  end;

implementation

{$R *.dfm}
constructor TfrSearchResultsItem.Create(
  AOwner: TComponent;
  AParent:TWinControl;
  APlacemark: IGeoCodePlacemark;
  AViewPortState: IViewPortState;
  AIntrnalBrowser: IInternalBrowser;
  AMapGoto: IMapViewGoto
);
begin
  inherited Create(AOwner);
  Parent:=AParent;
  FPlacemark:=APlacemark;
  FIntrnalBrowser := AIntrnalBrowser;
  LabelCaption.Caption:=FPlacemark.GetAddress;
  LabelDesc.Caption:=FPlacemark.GetDesc;
  FMapGoto:=AMapGoto;
  FViewPortState := AViewPortState;
  PanelFullDesc.Visible:=FPlacemark.GetFullDesc<>'';
  PanelDesc.Visible:=FPlacemark.GetDesc<>'';
end;

procedure TfrSearchResultsItem.LabelCaptionClick(Sender: TObject);
begin
  FMapGoto.GotoPos(FPlacemark.GetPoint, FViewPortState.GetCurrentZoom);
end;

procedure TfrSearchResultsItem.LabelDescDblClick(Sender: TObject);
begin
  FMapGoto.GotoPos(FPlacemark.GetPoint, FViewPortState.GetCurrentZoom);
end;

procedure TfrSearchResultsItem.LabelFullDescMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FIntrnalBrowser.ShowMessage(FPlacemark.GetAddress, FPlacemark.GetFullDesc);
end;

end.
