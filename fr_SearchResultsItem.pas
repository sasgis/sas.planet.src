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
  Types,
  Forms,
  Controls,
  Menus,
  ExtCtrls,
  StdCtrls,
  i_GeoCoder,
  i_VectorDataItemSimple,
  i_LocalCoordConverterChangeable,
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
    procedure FrameContextPopup(Sender: TObject; MousePos: TPoint; var Handled:
        Boolean);
    procedure LabelFullDescMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelCaptionClick(Sender: TObject);
    procedure LabelDescDblClick(Sender: TObject);
  private
    FPlacemark: IVectorDataItemPoint;
    FViewPortState: ILocalCoordConverterChangeable;
    FMapGoto: IMapViewGoto;
    FIntrnalBrowser: IInternalBrowser;
    FPopUp: TPopupMenu;
  public
    constructor Create(
      AOwner: TComponent;
      AParent:TWinControl;
      APopUp: TPopupMenu;
      const APlacemark: IVectorDataItemPoint;
      const AViewPortState: ILocalCoordConverterChangeable;
      const AIntrnalBrowser: IInternalBrowser;
      const AMapGoto: IMapViewGoto
    ); reintroduce;
  end;

implementation

{$R *.dfm}
constructor TfrSearchResultsItem.Create(
  AOwner: TComponent;
  AParent:TWinControl;
  APopUp: TPopupMenu;
  const APlacemark: IVectorDataItemPoint;
  const AViewPortState: ILocalCoordConverterChangeable;
  const AIntrnalBrowser: IInternalBrowser;
  const AMapGoto: IMapViewGoto
);
begin
  inherited Create(AOwner);
  Parent:=AParent;
  FPlacemark:=APlacemark;
  FPopUp := APopUp;
  FIntrnalBrowser := AIntrnalBrowser;
  LabelCaption.Caption:=FPlacemark.Name;
  LabelDesc.Caption:=FPlacemark.GetDesc;
  FMapGoto:=AMapGoto;
  FViewPortState := AViewPortState;
  PanelFullDesc.Visible:=FPlacemark.GetInfoHTML<>'';
  PanelDesc.Visible:=FPlacemark.GetDesc<>'';
end;

procedure TfrSearchResultsItem.FrameContextPopup(
  Sender: TObject;
  MousePos: TPoint;
  var Handled: Boolean
);
var
  VPoint: TPoint;
begin
  if FPopUp <> nil then begin
    if FPopUp.Tag <> 0 then begin
      IInterface(FPopUp.Tag)._Release;
    end;
    FPopUp.Tag := Integer(FPlacemark);
    IInterface(FPopUp.Tag)._AddRef;
    VPoint := ClientToScreen(MousePos);
    FPopUp.Popup(VPoint.X, VPoint.Y);
    Handled := True;
  end;
end;

procedure TfrSearchResultsItem.LabelCaptionClick(Sender: TObject);
begin
  FMapGoto.GotoPos(FPlacemark.GetPoint.Point, FViewPortState.GetStatic.Zoom, True);
end;

procedure TfrSearchResultsItem.LabelDescDblClick(Sender: TObject);
begin
  FMapGoto.GotoPos(FPlacemark.GetPoint.Point, FViewPortState.GetStatic.Zoom, True);
end;

procedure TfrSearchResultsItem.LabelFullDescMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FIntrnalBrowser.ShowMessage(FPlacemark.GetInfoCaption, FPlacemark.GetInfoHTML);
end;

end.
