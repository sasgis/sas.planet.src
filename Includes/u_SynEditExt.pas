unit u_SynEditExt;
 
interface

uses
  Classes,
  SynEdit;
 
type
  TSynEditBuilder = class
  public
    class function SynEditWithPasHighlighter(AOwner: TComponent): TSynEdit;
    class function SynEditWithIniHighlighter(AOwner: TComponent): TSynEdit;
    class function SynEditWithHtmlHighlighter(AOwner: TComponent): TSynEdit;
  end;
 
implementation

uses
  SysUtils,
  ActnList,
  Menus,
  SynHighlighterPas,
  SynHighlighterIni,
  SynHighlighterHtml,
  SynEditHighlighter;

type
  TSynEditExt = class(TSynEdit)
  private
    FActnList: TActionList;
    FPopupMenu: TPopupMenu;
    procedure CreateActns;
    procedure FillPopupMenu(APopupMenu: TPopupMenu);
    procedure CutExecute(Sender: TObject);
    procedure CutUpdate(Sender: TObject);
    procedure CopyExecute(Sender: TObject);
    procedure CopyUpdate(Sender: TObject);
    procedure PasteExecute(Sender: TObject);
    procedure PasteUpdate(Sender: TObject);
    procedure DeleteExecute(Sender: TObject);
    procedure DeleteUpdate(Sender: TObject);
    procedure SelectAllExecute(Sender: TObject);
    procedure SelectAllUpdate(Sender: TObject);
    procedure RedoExecute(Sender: TObject);
    procedure RedoUpdate(Sender: TObject);
    procedure UndoExecute(Sender: TObject);
    procedure UndoUpdate(Sender: TObject);
    procedure SetPopupMenuExt(const Value: TPopupMenu);
    function  GetPopupMenuExt: TPopupMenu;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PopupMenu: TPopupMenu read GetPopupMenuExt write SetPopupMenuExt;
  end;

{ TSynEditExt }

const
  cMenuName='SynEditExtPopupMenu';
 
procedure TSynEditExt.CopyExecute(Sender: TObject);
begin
  Self.CopyToClipboard;
end;
 
procedure TSynEditExt.CopyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Self.SelAvail;
end;
 
procedure TSynEditExt.CutExecute(Sender: TObject);
begin
  Self.CutToClipboard;
end;
 
procedure TSynEditExt.CutUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Self.SelAvail and not Self.ReadOnly;
end;
 
procedure TSynEditExt.DeleteExecute(Sender: TObject);
begin
  Self.SelText := '';
end;
 
procedure TSynEditExt.DeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Self.SelAvail and not Self.ReadOnly;
end;
 
procedure TSynEditExt.PasteExecute(Sender: TObject);
begin
  Self.PasteFromClipboard;
end;
 
procedure TSynEditExt.PasteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Self.CanPaste;
end;
 
procedure TSynEditExt.RedoExecute(Sender: TObject);
begin
  Self.Redo;
end;
 
procedure TSynEditExt.RedoUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Self.CanRedo;
end;
 
procedure TSynEditExt.SelectAllExecute(Sender: TObject);
begin
  Self.SelectAll;
end;
 
procedure TSynEditExt.SelectAllUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Self.Lines.Text<>'';
end;
 
procedure TSynEditExt.UndoExecute(Sender: TObject);
begin
  Self.Undo;
end;
 
procedure TSynEditExt.UndoUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Self.CanUndo;
end;
 
constructor TSynEditExt.Create(AOwner: TComponent);
begin
  inherited;
  FActnList := TActionList.Create(Self);
  FPopupMenu := TPopupMenu.Create(Self);
  FPopupMenu.Name := cMenuName;
  CreateActns;
  FillPopupMenu(FPopupMenu);
  PopupMenu := FPopupMenu;
end;
 
procedure TSynEditExt.CreateActns;
 
  procedure AddActItem(const AText: string; AShortCut: TShortCut; AEnabled: Boolean; OnExecute, OnUpdate: TNotifyEvent);
  var
    ActionItem  : TAction;
  begin
    ActionItem := TAction.Create(FActnList);
    ActionItem.ActionList := FActnList;
    ActionItem.Caption := AText;
    ActionItem.ShortCut := AShortCut;
    ActionItem.Enabled := AEnabled;
    ActionItem.OnExecute := OnExecute;
    ActionItem.OnUpdate := OnUpdate;
  end;
 
begin
  AddActItem('&Undo', Menus.ShortCut(Word('Z'), [ssCtrl]), False, UndoExecute, UndoUpdate);
  AddActItem('&Redo', Menus.ShortCut(Word('Y'), [ssCtrl]), False, RedoExecute, RedoUpdate);
  AddActItem('-', 0, False, nil, nil);
  AddActItem('Cu&t', Menus.ShortCut(Word('X'), [ssCtrl]), False, CutExecute, CutUpdate);
  AddActItem('&Copy', Menus.ShortCut(Word('C'), [ssCtrl]), False, CopyExecute, CopyUpdate);
  AddActItem('&Paste', Menus.ShortCut(Word('V'), [ssCtrl]), False, PasteExecute, PasteUpdate);
  AddActItem('De&lete', 0, False, DeleteExecute, DeleteUpdate);
  AddActItem('-', 0, False, nil, nil);
  AddActItem('Select &All', Menus.ShortCut(Word('A'), [ssCtrl]), False, SelectAllExecute, SelectAllUpdate);
end;
 
procedure TSynEditExt.SetPopupMenuExt(const Value: TPopupMenu);
var
  VMenuItem : TMenuItem;
begin
  SynEdit.TSynEdit(Self).PopupMenu := Value;
  if CompareText(cMenuName, Value.Name) <> 0 then begin
    VMenuItem := TMenuItem.Create(Value);
    VMenuItem.Caption := '-';
    Value.Items.Add(VMenuItem);
    FillPopupMenu(Value);
  end;
end;
 
function TSynEditExt.GetPopupMenuExt: TPopupMenu;
begin
  Result := SynEdit.TSynEdit(Self).PopupMenu;
end;

destructor TSynEditExt.Destroy;
begin
  FPopupMenu.Free;
  FActnList.Free;
  inherited;
end;

procedure TSynEditExt.FillPopupMenu(APopupMenu : TPopupMenu);
var
  I: Integer;
  VMenuItem: TMenuItem;
begin
  if Assigned(FActnList) then begin
    for I := 0 to FActnList.ActionCount-1 do begin
      VMenuItem := TMenuItem.Create(APopupMenu);
      VMenuItem.Action := FActnList.Actions[I];
      APopupMenu.Items.Add(VMenuItem);
    end;
  end;
end;

{ TSynEditBuilder }

const
  cNumber = $000080FF;
  cString = $00808080;
  cComment = $00008000;
  cSection = $00FF0080;
  cInstructionWord = $00FF0000;

class function TSynEditBuilder.SynEditWithPasHighlighter(AOwner: TComponent): TSynEdit;
var
  VHighlighter: TSynPasSyn;
begin
  Result := TSynEditExt.Create(AOwner);
  VHighlighter := TSynPasSyn.Create(AOwner);
  with VHighlighter do begin
    CommentAttri.Foreground := cComment;
    KeyAttri.Foreground := cInstructionWord;
    NumberAttri.Foreground := cNumber;
    FloatAttri.Foreground := cNumber;
    HexAttri.Foreground := cNumber;
    StringAttri.Foreground := cString;
    CharAttri.Foreground := cString;
  end;
  Result.Highlighter := VHighlighter;
end;

class function TSynEditBuilder.SynEditWithIniHighlighter(AOwner: TComponent): TSynEdit;
var
  VHighlighter: TSynIniSyn;
begin
  Result := TSynEditExt.Create(AOwner);
  VHighlighter := TSynIniSyn.Create(AOwner);
  with VHighlighter do begin
    CommentAttri.Foreground := cComment;
    SectionAttri.Foreground := cSection;
    NumberAttri.Foreground := cNumber;
    StringAttri.Foreground := cString;
  end;
  Result.Highlighter := VHighlighter;
end;

class function TSynEditBuilder.SynEditWithHtmlHighlighter(AOwner: TComponent): TSynEdit;
var
  VHighlighter: TSynHTMLSyn;
begin
  Result := TSynEditExt.Create(AOwner);
  VHighlighter := TSynHTMLSyn.Create(AOwner);
  Result.Highlighter := VHighlighter;
end;
 
end.