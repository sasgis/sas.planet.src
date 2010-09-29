unit fr_MarkDescription;

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
  TB2Item,
  TBX,
  TB2Dock,
  TB2Toolbar,
  StdCtrls,
  ExtCtrls,
  u_CommonFormAndFrameParents;

type
  TfrMarkDescription = class(TFrame)
    EditComment: TMemo;
    pnlDescriptionTop: TPanel;
    Label2: TLabel;
    TBXToolbar1: TTBXToolbar;
    TBXItem3: TTBXItem;
    TBXItem2: TTBXItem;
    TBXItem1: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    TBXItem4: TTBXItem;
    TBXItem5: TTBXItem;
    TBXItem6: TTBXItem;
    TBXSeparatorItem2: TTBXSeparatorItem;
    TBXItem7: TTBXItem;
    procedure TBXItem1Click(Sender: TObject);
    procedure EditCommentKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    function GetDescription: string;
    procedure SetDescription(const Value: string);
  public
    property Description: string read GetDescription write SetDescription;
  end;

implementation

uses
  Unit1;

{$R *.dfm}

type
  TEditBtn = (ebB,ebI,ebU,ebLeft,ebCenter,ebRight,ebImg);

procedure TfrMarkDescription.EditCommentKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var s:string;
    seli:integer;
begin
  if Key=13 then begin
    Key:=0;
    s:=EditComment.Text;
    seli:=EditComment.SelStart;
    Insert('<BR>',s,EditComment.SelStart+1);
    EditComment.Text:=s;
    EditComment.SelStart:=seli+4;
  end;
end;

function TfrMarkDescription.GetDescription: string;
begin
  Result := EditComment.Text;
end;

procedure TfrMarkDescription.SetDescription(const Value: string);
begin
  EditComment.Text := Value;
end;

procedure TfrMarkDescription.TBXItem1Click(Sender: TObject);
var s:string;
    seli:integer;
begin
 s:=EditComment.Text;
 seli:=EditComment.SelStart;
 case TEditBtn(TTBXItem(sender).Tag) of
  ebB: begin
        Insert('<b>',s,EditComment.SelStart+1);
        Insert('</b>',s,EditComment.SelStart+EditComment.SelLength+3+1);
       end;
  ebI: begin
        Insert('<i>',s,EditComment.SelStart+1);
        Insert('</i>',s,EditComment.SelStart+EditComment.SelLength+3+1);
       end;
  ebU: begin
        Insert('<u>',s,EditComment.SelStart+1);
        Insert('</u>',s,EditComment.SelStart+EditComment.SelLength+3+1);
       end;
  ebImg:
       if (FMain.OpenPictureDialog.Execute)and(FMain.OpenPictureDialog.FileName<>'') then begin
         Insert('<img src="'+FMain.OpenPictureDialog.FileName+'"/>',s,EditComment.SelStart+1);
       end;
  ebCenter:
       begin
        Insert('<CENTER>',s,EditComment.SelStart+1);
        Insert('</CENTER>',s,EditComment.SelStart+EditComment.SelLength+8+1);
       end;
  ebLeft:
       begin
        Insert('<div ALIGN=LEFT>',s,EditComment.SelStart+1);
        Insert('</div>',s,EditComment.SelStart+EditComment.SelLength+16+1);
       end;
  ebRight:
       begin
        Insert('<div ALIGN=RIGHT>',s,EditComment.SelStart+1);
        Insert('</div>',s,EditComment.SelStart+EditComment.SelLength+17+1);
       end;
 end;
 EditComment.Text:=s;
 EditComment.SelStart:=seli;
end;

end.
