unit USearchResult;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  UGeoFun,
  Unit1,
  t_GeoTypes;

type
  TFSearchResult = class(TForm)
    ListBox1: TListBox;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

TSearthResult = class
 text:string;
 ll:TExtendedPoint;
end;

var
  FSearchResult: TFSearchResult;

implementation

{$R *.dfm}

procedure TFSearchResult.Button1Click(Sender: TObject);
begin
 if assigned(ListBox1.Items.Objects[ListBox1.ItemIndex])
  then Fmain.topos(TSearthResult(ListBox1.Items.Objects[ListBox1.ItemIndex]).ll.y,
                   TSearthResult(ListBox1.Items.Objects[ListBox1.ItemIndex]).ll.y,
                   zoom_size, true);
end;

end.
