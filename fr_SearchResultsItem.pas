{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit fr_SearchResultsItem;

interface

uses
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  Types,
  i_GeoCoder,
  i_ViewPortState,
  i_MapViewGoto,
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
  public
    constructor Create(
      AOwner: TComponent;
      AParent:TWinControl;
      APlacemark: IGeoCodePlacemark;
      AViewPortState: IViewPortState;
      AMapGoto: IMapViewGoto
    ); reintroduce;
  end;

implementation

uses
  frm_IntrnalBrowser;

{$R *.dfm}
constructor TfrSearchResultsItem.Create(
  AOwner: TComponent;
  AParent:TWinControl;
  APlacemark: IGeoCodePlacemark;
  AViewPortState: IViewPortState;
  AMapGoto: IMapViewGoto
);
begin
  inherited Create(AOwner);
  Parent:=AParent;
  FPlacemark:=APlacemark;
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
  frmIntrnalBrowser.showmessage(FPlacemark.GetAddress,FPlacemark.GetFullDesc);
end;

end.
