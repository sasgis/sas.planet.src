{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit fr_SearchResultsItem;

interface

uses
  Types,
  Classes,
  Forms,
  Controls,
  Menus,
  ExtCtrls,
  StdCtrls,
  SysUtils,
  GR32_Image,
  i_VectorDataItemSimple,
  i_ValueToStringConverter,
  i_MapViewGoto,
  i_InternalBrowser;

type
  TfrSearchResultsItem = class(TFrame)
    PanelCaption: TPanel;
    PanelFullDescImg: TPanel;
    PanelDesc: TPanel;
    LabelDesc: TLabel;
    LabelFullDescImg: TLabel;
    Bevel1: TBevel;
    LabelCaption: TLabel;
    imgIcon: TImage32;
    LabelMarkInfo: TLabel;
    PanelFullDescShort: TPanel;
    LabelFullDescShort: TLabel;
    PanelCategory: TPanel;
    LabelCategory: TLabel;
    procedure FrameContextPopup(Sender: TObject; MousePos: TPoint; var Handled:
        Boolean);
    procedure LabelFullDescImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelCaptionClick(Sender: TObject);
    procedure LabelDescDblClick(Sender: TObject);
  private
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FPlacemark: IVectorDataItemSimple;
    FMapGoto: IMapViewGoto;
    FIntrnalBrowser: IInternalBrowser;
    FPopUp: TPopupMenu;
  public
    constructor Create(
      AOwner: TComponent;
      AParent:TWinControl;
      APopUp: TPopupMenu;
      const APlacemark: IVectorDataItemSimple;
      const AIntrnalBrowser: IInternalBrowser;
      const AMapGoto: IMapViewGoto;
      const AValueConverterConfig: IValueToStringConverterConfig
    ); reintroduce;
  end;

implementation
uses
  t_GeoTypes,
  i_AppearanceOfVectorItem,
  u_BitmapFunc;

{$R *.dfm}
constructor TfrSearchResultsItem.Create(
  AOwner: TComponent;
  AParent:TWinControl;
  APopUp: TPopupMenu;
  const APlacemark: IVectorDataItemSimple;
  const AIntrnalBrowser: IInternalBrowser;
  const AMapGoto: IMapViewGoto;
  const AValueConverterConfig: IValueToStringConverterConfig
);
var
  VAppearanceIcon: IAppearancePointIcon;
  VGotoLonLat: TDoublePoint;
  VValueConverter: IValueToStringConverter;
  VItemWithCategory: IVectorDataItemWithCategory;
  VCategoryName: string;
begin
  inherited Create(AOwner);
  FValueToStringConverterConfig := AValueConverterConfig;
  Parent:=AParent;
  FPlacemark:=APlacemark;
  FPopUp := APopUp;
  FIntrnalBrowser := AIntrnalBrowser;
  LabelCaption.Caption := FPlacemark.Name;
  LabelDesc.Caption := FPlacemark.GetDesc;
  FMapGoto := AMapGoto;
  PanelDesc.Visible := FPlacemark.GetDesc <> '';
  LabelFullDescImg.Visible := FPlacemark.GetInfoHTML <> ''; // есть ли вообще доп инфо...
  LabelFullDescShort.Visible := FPlacemark.GetInfoHTML <> '';
  VValueConverter := FValueToStringConverterConfig.GetStatic;

  if Supports(FPlacemark.MainInfo, IVectorDataItemWithCategory, VItemWithCategory) then begin
    if VItemWithCategory.Category <> nil then begin
      LabelCategory.Caption := VItemWithCategory.Category.Name;
    end;
  end;
  PanelCategory.Visible := LabelCategory.Caption <> '';

  if Supports(FPlacemark.Appearance, IAppearancePointIcon, VAppearanceIcon) then begin
    if Assigned(VAppearanceIcon.Pic) then begin
      imgIcon.Bitmap.SetSizeFrom(imgIcon);
      imgIcon.Visible := True;
      CopyBitmap32StaticToBitmap32(VAppearanceIcon.Pic.GetMarker, imgIcon.Bitmap);
      VGotoLonLat := FPlacemark.Geometry.GetGoToLonLat;
      LabelMarkInfo.Caption := '[ '+VValueConverter.LonLatConvert(VGotoLonLat) + ' ]';
    end;
  end;
  PanelFullDescImg.Visible := imgIcon.Visible;

  if imgIcon.Visible then begin // если еть картинка - вырубаем тонкую панель Full Description
    PanelFullDescShort.Visible := False;
  end else begin // иначе показываем панель при наличии ссылки
    PanelFullDescShort.Visible := LabelFullDescShort.Visible;
  end;
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
  FMapGoto.FitRectToScreen(FPlacemark.Geometry.Bounds.Rect);
  FMapGoto.ShowMarker(FPlacemark.Geometry.GetGoToLonLat);
end;

procedure TfrSearchResultsItem.LabelDescDblClick(Sender: TObject);
begin
  FMapGoto.FitRectToScreen(FPlacemark.Geometry.Bounds.Rect);
  FMapGoto.ShowMarker(FPlacemark.Geometry.GetGoToLonLat);
end;

procedure TfrSearchResultsItem.LabelFullDescImgMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FIntrnalBrowser.ShowMessage(FPlacemark.GetInfoCaption, FPlacemark.GetInfoHTML);
end;

end.
