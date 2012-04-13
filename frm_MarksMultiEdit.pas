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

unit frm_MarksMultiEdit;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  Spin,
  ExtCtrls,  
  GR32,
  u_CommonFormAndFrameParents,
  i_MarkCategory,
  i_LanguageManager,
  i_ImportConfig,
  i_MarksDb,
  i_MarkCategoryDB,
  fr_MarksGeneralOptions,
  fr_MarkCategorySelectOrAdd;

type
  TfrmMarksMultiEdit = class(TFormWitghLanguageManager)
    btnOk: TButton;
    btnCancel: TButton;
    pnlMarksGeneralOptions: TPanel;
    pnlCategory: TPanel;
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    frMarkCategory: TfrMarkCategorySelectOrAdd;
    frMarksGeneralOptions: TfrMarksGeneralOptions;
    FCategoryDB: IMarkCategoryDB;
    FMarksDb: IMarksDb;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      ACategoryDB: IMarkCategoryDB;
      AMarksDb: IMarksDb
    ); reintroduce;
    destructor Destroy; override;
    function GetImportConfig(ACategory:ICategory): IImportConfig;
    procedure RefreshTranslation; override;
  end;

implementation

uses
  i_MarkTemplate,
  i_MarkPicture,
  u_ImportConfig;

{$R *.dfm}

constructor TfrmMarksMultiEdit.Create(
  ALanguageManager: ILanguageManager;
  ACategoryDB: IMarkCategoryDB;
  AMarksDb: IMarksDb
);
begin
  inherited Create(ALanguageManager);
  FMarksDb := AMarksDb;
  FCategoryDB := ACategoryDB;

  frMarkCategory :=
    TfrMarkCategorySelectOrAdd.Create(
      nil,
      FCategoryDB
    );

  frMarksGeneralOptions:= TfrMarksGeneralOptions.Create(nil);
  frMarksGeneralOptions.chkPointIgnore.Checked:=true;
  frMarksGeneralOptions.chkLineIgnore.Checked:=true;
  frMarksGeneralOptions.chkPolyIgnore.Checked:=true;
end;

destructor TfrmMarksMultiEdit.Destroy;
begin
  FreeAndNil(frMarksGeneralOptions);
  FreeAndNil(frMarkCategory);
  inherited;
end;

function TfrmMarksMultiEdit.GetImportConfig(ACategory:ICategory): IImportConfig;
var
  VIndex: Integer;
  VPic: IMarkPicture;
  VMarkTemplatePoint: IMarkTemplatePoint;
  VMarkTemplateLine: IMarkTemplateLine;
  VMarkTemplatePoly: IMarkTemplatePoly;
  VCategory: ICategory;
begin
    frMarksGeneralOptions.Init(FMarksDb);
    frMarkCategory.Init(ACategory);
    try
      if ShowModal = mrOk then begin
        if not frMarksGeneralOptions.chkPointIgnore.Checked then begin
          VIndex := frMarksGeneralOptions.cbbPointIcon.ItemIndex;
          if VIndex < 0 then begin
            VPic := nil;
          end else begin
            VPic := IMarkPicture(Pointer(frMarksGeneralOptions.cbbPointIcon.Items.Objects[VIndex]));
          end;
          VCategory := frMarkCategory.GetCategory;
          VMarkTemplatePoint :=
            FMarksDb.Factory.Config.PointTemplateConfig.CreateTemplate(
              VPic,
              VCategory,
              SetAlpha(Color32(frMarksGeneralOptions.clrbxPointTextColor.Selected),round(((100-frMarksGeneralOptions.sePointTextTransp.Value)/100)*256)),
              SetAlpha(Color32(frMarksGeneralOptions.clrbxPointShadowColor.Selected),round(((100-frMarksGeneralOptions.sePointTextTransp.Value)/100)*256)),
              frMarksGeneralOptions.sePointFontSize.Value,
              frMarksGeneralOptions.sePointIconSize.Value
            );
        end;
        VMarkTemplateLine := nil;
        if not frMarksGeneralOptions.chkLineIgnore.Checked then begin
          VCategory := frMarkCategory.GetCategory;
          VMarkTemplateLine :=
            FMarksDb.Factory.Config.LineTemplateConfig.CreateTemplate(
              VCategory,
              SetAlpha(Color32(frMarksGeneralOptions.clrbxLineColor.Selected),round(((100-frMarksGeneralOptions.seLineTransp.Value)/100)*256)),
              frMarksGeneralOptions.seLineWidth.Value
            );
        end;
        VMarkTemplatePoly := nil;
        if not frMarksGeneralOptions.chkPolyIgnore.Checked then begin
          VCategory := frMarkCategory.GetCategory;
          VMarkTemplatePoly :=
            FMarksDb.Factory.Config.PolyTemplateConfig.CreateTemplate(
              VCategory,
              SetAlpha(Color32(frMarksGeneralOptions.clrbxPolyLineColor.Selected),round(((100-frMarksGeneralOptions.sePolyLineTransp.Value)/100)*256)),
              SetAlpha(Color32(frMarksGeneralOptions.clrbxPolyFillColor.Selected),round(((100-frMarksGeneralOptions.sePolyFillTransp.Value)/100)*256)),
              frMarksGeneralOptions.sePolyLineWidth.Value
            );
        end;
        Result :=
          TImportConfig.Create(
            FMarksDb,
            VMarkTemplatePoint,
            VMarkTemplateLine,
            VMarkTemplatePoly
          );
      end else begin
        Result := nil;
      end;
    finally
      frMarkCategory.Clear;
      frMarksGeneralOptions.Clear;
    end;
end;

procedure TfrmMarksMultiEdit.RefreshTranslation;
begin
  inherited;
  frMarkCategory.RefreshTranslation;
  frMarksGeneralOptions.RefreshTranslation;
end;

procedure TfrmMarksMultiEdit.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmMarksMultiEdit.FormShow(Sender: TObject);
begin
  frMarkCategory.Parent := pnlCategory;
  frMarksGeneralOptions.Parent := pnlMarksGeneralOptions;
end;

end.
