unit fr_SearchResultsItem;

interface

uses
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  Types,
  I_GeoCoder,
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
  private
    FPlacemark: IGeoCodePlacemark;
    FMapGoto: IMapViewGoto;
  public
    constructor Create(
      AOwner: TComponent;
      AParent:TWinControl; 
      APlacemark: IGeoCodePlacemark;
      AMapGoto: IMapViewGoto
    ); reintroduce;
  end;

implementation

uses
  frm_IntrnalBrowser,
  u_GlobalState;

{$R *.dfm}
constructor TfrSearchResultsItem.Create(AOwner: TComponent; AParent:TWinControl; APlacemark: IGeoCodePlacemark; AMapGoto: IMapViewGoto);
begin
  inherited Create(AOwner);
  Parent:=AParent;
  FPlacemark:=APlacemark;
  LabelCaption.Caption:=FPlacemark.GetAddress;
  LabelDesc.Caption:=FPlacemark.GetDesc;
  FMapGoto:=AMapGoto;
  PanelFullDesc.Visible:=FPlacemark.GetFullDesc<>'';
  PanelDesc.Visible:=FPlacemark.GetDesc<>'';
end;

procedure TfrSearchResultsItem.LabelCaptionClick(Sender: TObject);
begin
  FMapGoto.GotoPos(FPlacemark.GetPoint, GState.MainFormConfig.ViewPortState.GetCurrentZoom);
end;

procedure TfrSearchResultsItem.LabelFullDescMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  frmIntrnalBrowser.showmessage(FPlacemark.GetAddress,FPlacemark.GetFullDesc);
end;

end.
