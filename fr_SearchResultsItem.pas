unit fr_SearchResultsItem;

interface

uses
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  Types,
  Classes;

type
  TfrSearchResultsItem = class(TFrame)
    PanelCaption: TPanel;
    PanelFullDesc: TPanel;
    PanelDesc: TPanel;
    LabelCaption: TLabel;
    LabelDesc: TLabel;
    LabelFullDesc: TLabel;
    procedure LabelFullDescMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FFullDesc:string;
  public
    constructor Create(
      AOwner: TComponent;
      ACaption: string;
      ADesc: string;
      AFullDesc: string
    ); reintroduce;
  end;

implementation

uses
  frm_IntrnalBrowser;

{$R *.dfm}
constructor TfrSearchResultsItem.Create(AOwner: TComponent; ACaption: string; ADesc: string; AFullDesc: string);
begin
  inherited Create(AOwner);
  LabelCaption.Caption:=ACaption;
  LabelDesc.Caption:=ADesc;
  FFullDesc:=AFullDesc;
end;

procedure TfrSearchResultsItem.LabelFullDescMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  frmIntrnalBrowser.showmessage(LabelCaption.Caption,FFullDesc);
end;

end.
