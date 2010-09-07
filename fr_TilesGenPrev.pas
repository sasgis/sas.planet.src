unit fr_TilesGenPrev;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, CheckLst, ExtCtrls;

type
  TfrTilesGenPrev = class(TFrame)
    pnlBottom: TPanel;
    pnlRight: TPanel;
    pnlCenter: TPanel;
    lblMap: TLabel;
    lblStat: TLabel;
    cbbMap: TComboBox;
    pnlTop: TPanel;
    bvlTop: TBevel;
    lblCaption: TLabel;
    ComboBox: TComboBox;
    lblFromZoom: TLabel;
    chkAllZooms: TCheckBox;
    lblZooms: TLabel;
    chklstZooms: TCheckListBox;
    cbbResampler: TComboBox;
    lblResampler: TLabel;
    chkReplace: TCheckBox;
    chkSaveFullOnly: TCheckBox;
    chkFromPrevZoom: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
