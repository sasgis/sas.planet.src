unit fr_SearchResultsItem;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrSearchResultsItem = class(TFrame)
    PanelCaption: TPanel;
    PanelFullDesc: TPanel;
    PanelDesc: TPanel;
    LabelCaption: TLabel;
    LabelDesc: TLabel;
    LabelFullDesc: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
