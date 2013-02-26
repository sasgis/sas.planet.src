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

unit fr_MarkDescription;

interface

uses
  Windows,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ExtDlgs,
  TBX,
  TBXGraphics,
  ImgList,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  i_LanguageManager,
  i_PathConfig,
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
    tbxtmInsertUrl: TTBXItem;
    imglstToolbar: TTBXImageList;
    OpenPictureDialog: TOpenPictureDialog;
    procedure TBXItem1Click(Sender: TObject);
    procedure EditCommentKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FMediaPath: IPathConfig;

    function GetDescription: string;
    procedure SetDescription(const Value: string);
  public
    property Description: string read GetDescription write SetDescription;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMediaPath: IPathConfig
    ); reintroduce;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  c_InternalBrowser;

{$R *.dfm}

type
  TEditBtn = (ebB,ebI,ebU,ebLeft,ebCenter,ebRight,ebImg, ebUrl);

constructor TfrMarkDescription.Create(
  const ALanguageManager: ILanguageManager;
  const AMediaPath: IPathConfig
);
begin
  inherited Create(ALanguageManager);
  FMediaPath := AMediaPath;
end;

procedure TfrMarkDescription.EditCommentKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  s:string;
  VSelStart:integer;
  Form: TCustomForm;
begin
  if (Key=VK_RETURN) then begin
    if (ssCtrl in Shift) then begin
      Key:=0;
      s:=EditComment.Text;
      VSelStart:=EditComment.SelStart;
      Insert('<BR>', s, VSelStart+1);
      EditComment.Text := s;
      EditComment.SelStart := VSelStart+4;
    end;
  end else if Key = VK_ESCAPE then begin
    Form := GetParentForm(Self);
    if Form <> nil then Form.ModalResult := mrCancel;
  end else if (Key = $41) and (ssCtrl in Shift) then begin
    EditComment.SelectAll;
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
var
  s:string;
  VSelStart:integer;
  VSelLen:integer;
  VSelectedText: string;
  VTextBeforeSelection: string;
  VTextAfterSelection: string;
  VImageUrl: string;
  VMediaPath: string;
  VFileName: string;
begin
  s := EditComment.Text;
  VSelStart := EditComment.SelStart;
  VSelLen := EditComment.SelLength;
  VSelectedText := EditComment.SelText;
  VTextBeforeSelection := '';
  VTextAfterSelection := '';
  case TEditBtn(TTBXItem(Sender).Tag) of
  ebB: begin
        VTextBeforeSelection := '<b>';
        VTextAfterSelection := '</b>';
       end;
  ebI: begin
        VTextBeforeSelection := '<i>';
        VTextAfterSelection := '</i>';
       end;
  ebU: begin
        VTextBeforeSelection := '<u>';
        VTextAfterSelection := '</u>';
       end;
  ebUrl: begin
        if VSelLen = 0 then begin
          VTextBeforeSelection := '<a href=""></a>';
          VTextAfterSelection := '';
        end else begin
          VTextBeforeSelection := '<a href="'+VSelectedText+'">';
          VTextAfterSelection := '</a>';
        end;
       end;
  ebImg:
       if (OpenPictureDialog.Execute)and(OpenPictureDialog.FileName<>'') then begin
          VImageUrl := OpenPictureDialog.FileName;
          VMediaPath := IncludeTrailingPathDelimiter(FMediaPath.FullPath);
          if LeftStr(VImageUrl, Length(VMediaPath)) = VMediaPath then begin
            VFileName := MidStr(VImageUrl, Length(VMediaPath) + 1, Length(VImageUrl) - Length(VMediaPath));
            if PathDelim <> '/' then begin
              VFileName := ReplaceStr(VFileName, PathDelim, '/');
            end;
            VImageUrl := CMediaDataInternalURL + VFileName;
          end;
          VTextBeforeSelection := '<img src="' + VImageUrl + '"/>';
          VTextAfterSelection := '';
       end;
  ebCenter:
       begin
        VTextBeforeSelection := '<CENTER>';
        VTextAfterSelection := '</CENTER>';
       end;
  ebLeft:
       begin
        VTextBeforeSelection := '<div ALIGN=LEFT>';
        VTextAfterSelection := '</div>';
       end;
  ebRight:
       begin
        VTextBeforeSelection := '<div ALIGN=RIGHT>';
        VTextAfterSelection := '</div>';
       end;
  end;
  if (VTextBeforeSelection <> '') or (VTextAfterSelection <> '') then begin
    Insert(VTextBeforeSelection, s, VSelStart+1);
    Insert(VTextAfterSelection, s, VSelStart + VSelLen+length(VTextBeforeSelection)+1);
    EditComment.Text:=s;
    EditComment.SelStart :=
      VSelStart +
      VSelLen +
      length(VTextBeforeSelection) +
      Length(VTextAfterSelection);
  end;

end;

end.
