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

unit frm_MarkEditPath;

interface

uses
  Windows,
  SysUtils,
  Buttons,
  Classes,
  Controls,
  Forms,
  Dialogs,
  Spin,
  StdCtrls,
  ExtCtrls,
  GR32,
  u_CommonFormAndFrameParents,
  i_PathConfig,
  i_LanguageManager,
  i_Mark,
  i_MarkCategoryDB,
  i_MarkFactory,
  fr_MarkDescription,
  fr_MarkCategorySelectOrAdd;

type
  TfrmMarkEditPath = class(TFormWitghLanguageManager)
    lblName: TLabel;
    lblLineColor: TLabel;
    lblWidth: TLabel;
    edtName: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    chkVisible: TCheckBox;
    clrbxLineColor: TColorBox;
    seWidth: TSpinEdit;
    SEtransp: TSpinEdit;
    lblTransp: TLabel;
    ColorDialog1: TColorDialog;
    btnLineColor: TSpeedButton;
    pnlCategory: TPanel;
    pnlName: TPanel;
    pnlDescription: TPanel;
    flwpnlStyle: TFlowPanel;
    pnlBottomButtons: TPanel;
    btnSetAsTemplate: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure btnLineColorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSetAsTemplateClick(Sender: TObject);
  private
    FCategoryDB: IMarkCategoryDB;
    FMarkFactory: IMarkFactory;
    frMarkDescription: TfrMarkDescription;
    frMarkCategory: TfrMarkCategorySelectOrAdd;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMediaPath: IPathConfig;
      const AMarkFactory: IMarkFactory;
      const ACategoryDB: IMarkCategoryDB
    ); reintroduce;
    destructor Destroy; override;
    function EditMark(
      const AMark: IMarkLine;
      const AIsNewMark: Boolean;
      var AVisible: Boolean
    ): IMarkLine;
  end;

implementation

uses
  i_MarkTemplate,
  i_MarkFactoryConfig,
  u_ResStrings;

{$R *.dfm}

constructor TfrmMarkEditPath.Create(
  const ALanguageManager: ILanguageManager;
  const AMediaPath: IPathConfig;
  const AMarkFactory: IMarkFactory;
  const ACategoryDB: IMarkCategoryDB
);
begin
  inherited Create(ALanguageManager);
  FCategoryDB := ACategoryDB;
  FMarkFactory := AMarkFactory;

  frMarkDescription := TfrMarkDescription.Create(ALanguageManager, AMediaPath);
  frMarkCategory :=
    TfrMarkCategorySelectOrAdd.Create(
      ALanguageManager,
      FCategoryDB
    );
end;

destructor TfrmMarkEditPath.Destroy;
begin
  FreeAndNil(frMarkDescription);
  FreeAndNil(frMarkCategory);
  inherited;
end;

function TfrmMarkEditPath.EditMark(
  const AMark: IMarkLine;
  const AIsNewMark: Boolean;
  var AVisible: Boolean
): IMarkLine;
begin
  frMarkCategory.Init(AMark.Category);
  try
    edtName.Text:=AMark.Name;
    frMarkDescription.Description := AMark.Desc;
    SEtransp.Value:=100-round(AlphaComponent(AMark.LineColor)/255*100);
    seWidth.Value:=AMark.LineWidth;
    clrbxLineColor.Selected:=WinColor(AMark.LineColor);
    chkVisible.Checked:= AVisible;
    if AIsNewMark then begin
      Caption := SAS_STR_AddNewPath;
    end else begin
      Caption := SAS_STR_EditPath;
    end;
    Self.PopupParent := Application.MainForm;
    if ShowModal=mrOk then begin
      Result :=
        FMarkFactory.ModifyLine(
          AMark,
          edtName.Text,
          frMarkCategory.GetCategory,
          frMarkDescription.Description,
          AMark.Line,
          SetAlpha(Color32(clrbxLineColor.Selected),round(((100-SEtransp.Value)/100)*256)),
          seWidth.Value
        );
      AVisible := chkVisible.Checked;
    end else begin
      Result := nil;
    end;
  finally
    frMarkCategory.Clear;
  end;
end;

procedure TfrmMarkEditPath.FormShow(Sender: TObject);
begin
  frMarkCategory.Parent := pnlCategory;
  frMarkDescription.Parent := pnlDescription;
  edtName.SetFocus;
end;

procedure TfrmMarkEditPath.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmMarkEditPath.btnSetAsTemplateClick(Sender: TObject);
var
  VConfig: IMarkLineTemplateConfig;
  VTemplate: IMarkTemplateLine;
begin
  if MessageBox(handle, pchar('Set as default for new marks?'), pchar(SAS_MSG_coution), 36) = IDYES then begin
    VConfig := FMarkFactory.Config.LineTemplateConfig;
    VTemplate :=
      VConfig.CreateTemplate(
        frMarkCategory.GetCategory,
        SetAlpha(Color32(clrbxLineColor.Selected),round(((100-SEtransp.Value)/100)*256)),
        seWidth.Value
      );
    VConfig.DefaultTemplate := VTemplate;
  end;
end;

procedure TfrmMarkEditPath.btnLineColorClick(Sender: TObject);
begin
 if ColorDialog1.Execute then clrbxLineColor.Selected:=ColorDialog1.Color;
end;

end.
