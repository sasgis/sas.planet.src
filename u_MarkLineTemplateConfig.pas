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

unit u_MarkLineTemplateConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_LanguageManager,
  i_MarkTemplate,
  i_MarkCategory,
  i_MarksFactoryConfig,
  i_MarkCategoryDBSmlInternal,
  u_MarkTemplateConfigBase;

type
  TMarkLineTemplateConfig = class(TMarkTemplateConfigBase, IMarkLineTemplateConfig)
  private
    FDefaultTemplate: IMarkTemplateLine;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function CreateTemplate(
      const ACategory: ICategory;
      AColor1: TColor32;
      AScale1: Integer
    ): IMarkTemplateLine;

    function GetDefaultTemplate: IMarkTemplateLine;
    procedure SetDefaultTemplate(const AValue: IMarkTemplateLine);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ACategoryDb: IMarkCategoryDBSmlInternal
    );
  end;

implementation

uses
  SysUtils,
  i_MarksDbSmlInternal,
  u_StringConfigDataElementWithDefByStringRec,
  u_ConfigProviderHelpers,
  u_ResStrings,
  u_MarkTemplates;

{ TMarkLineTemplateConfig }

constructor TMarkLineTemplateConfig.Create(
  const ALanguageManager: ILanguageManager;
  const ACategoryDb: IMarkCategoryDBSmlInternal
);
begin
  inherited Create(
    ACategoryDb,
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager,
      @SAS_STR_NewPath,
      True,
      'FormatString',
      True
    )
  );

  FDefaultTemplate := CreateTemplate(
    nil,
    SetAlpha(clRed32, 166),
    2
  );
end;

function TMarkLineTemplateConfig.CreateTemplate(
  const ACategory: ICategory;
  AColor1: TColor32;
  AScale1: Integer
): IMarkTemplateLine;
var
  VCategoryId: Integer;
  VCategoryInternal: IMarkCategorySMLInternal;
begin
  VCategoryId := -1;
  if ACategory <> nil then begin
    if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
      VCategoryId := VCategoryInternal.Id;
    end;
  end;
  Result := TMarkTemplateLine.Create(
    CategoryDb,
    NameGenerator,
    VCategoryId,
    AColor1,
    AScale1
  );
end;

procedure TMarkLineTemplateConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
var
  VCategoryId: Integer;
  VLineColor: TColor32;
  VLineWidth: Integer;
  VTemplateInternal: IMarkTemplateSMLInternal;
begin
  inherited;
  VCategoryID := -1;
  if Supports(FDefaultTemplate, IMarkTemplateSMLInternal, VTemplateInternal) then begin
    VCategoryId := VTemplateInternal.CategoryId;
  end;
  VLineColor := FDefaultTemplate.LineColor;
  VLineWidth := FDefaultTemplate.LineWidth;
  if AConfigData <> nil then begin
    VCategoryId := AConfigData.ReadInteger('CategoryId', VCategoryId);
    VLineColor := ReadColor32(AConfigData, 'LineColor', VLineColor);
    VLineWidth := AConfigData.ReadInteger('LineWidth', VLineWidth);
  end;
  SetDefaultTemplate(
    TMarkTemplateLine.Create(
      CategoryDb,
      NameGenerator,
      VCategoryId,
      VLineColor,
      VLineWidth
    )
  );
end;

procedure TMarkLineTemplateConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
var
  VCategoryId: Integer;
  VTemplateInternal: IMarkTemplateSMLInternal;
begin
  inherited;
  VCategoryID := -1;
  if Supports(FDefaultTemplate, IMarkTemplateSMLInternal, VTemplateInternal) then begin
    VCategoryId := VTemplateInternal.CategoryId;
  end;
  AConfigData.WriteInteger('CategoryId', VCategoryId);
  WriteColor32(AConfigData, 'LineColor', FDefaultTemplate.LineColor);
  AConfigData.WriteInteger('LineWidth', FDefaultTemplate.LineWidth);
end;

function TMarkLineTemplateConfig.GetDefaultTemplate: IMarkTemplateLine;
begin
  LockRead;
  try
    Result := FDefaultTemplate;
  finally
    UnlockRead;
  end;
end;

procedure TMarkLineTemplateConfig.SetDefaultTemplate(
  const AValue: IMarkTemplateLine
);
begin
  if AValue <> nil then begin
    LockWrite;
    try
      if (FDefaultTemplate = nil) or (not FDefaultTemplate.IsSame(AValue)) then begin
        FDefaultTemplate := AValue;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

end.

