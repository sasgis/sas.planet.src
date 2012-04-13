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

unit u_MarkPolyTemplateConfig;

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
  TMarkPolyTemplateConfig = class(TMarkTemplateConfigBase, IMarkPolyTemplateConfig)
  private
    FDefaultTemplate: IMarkTemplatePoly;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function CreateTemplate(
      ACategory: ICategory;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer
    ): IMarkTemplatePoly;

    function GetDefaultTemplate: IMarkTemplatePoly;
    procedure SetDefaultTemplate(AValue: IMarkTemplatePoly);
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      ACategoryDb: IMarkCategoryDBSmlInternal
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

{ TMarkPolyTemplateConfig }

constructor TMarkPolyTemplateConfig.Create(
  ALanguageManager: ILanguageManager;
  ACategoryDb: IMarkCategoryDBSmlInternal
);
begin
  inherited Create(
    ACategoryDb,
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager,
      @SAS_STR_NewPoly,
      True,
      'FormatString',
      True
    )
  );

  FDefaultTemplate := CreateTemplate(
    nil,
    SetAlpha(clBlack32, 166),
    SetAlpha(clWhite32, 51),
    2
  );
end;

function TMarkPolyTemplateConfig.CreateTemplate(
  ACategory: ICategory;
  AColor1: TColor32;
  AColor2: TColor32;
  AScale1: Integer
): IMarkTemplatePoly;
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
  Result := TMarkTemplatePoly.Create(
    CategoryDb,
    NameGenerator,
    VCategoryId,
    AColor1,
    AColor2,
    AScale1
  );
end;

procedure TMarkPolyTemplateConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
var
  VCategoryId: Integer;
  VBorderColor, VFillColor: TColor32;
  VLineWidth: Integer;
  VTemplateInternal: IMarkTemplateSMLInternal;
begin
  inherited;
  VCategoryID := -1;
  if Supports(FDefaultTemplate, IMarkTemplateSMLInternal, VTemplateInternal) then begin
    VCategoryId := VTemplateInternal.CategoryId;
  end;
  VBorderColor := FDefaultTemplate.BorderColor;
  VFillColor := FDefaultTemplate.FillColor;
  VLineWidth := FDefaultTemplate.LineWidth;
  if AConfigData <> nil then begin
    VCategoryId := AConfigData.ReadInteger('CategoryId', VCategoryId);
    VBorderColor := ReadColor32(AConfigData, 'LineColor', VBorderColor);
    VFillColor := ReadColor32(AConfigData, 'FillColor', VFillColor);
    VLineWidth := AConfigData.ReadInteger('LineWidth', VLineWidth);
  end;
  SetDefaultTemplate(
    TMarkTemplatePoly.Create(
      CategoryDb,
      NameGenerator,
      VCategoryId,
      VBorderColor,
      VFillColor,
      VLineWidth
    )
  );
end;

procedure TMarkPolyTemplateConfig.DoWriteConfig(
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
  WriteColor32(AConfigData, 'LineColor', FDefaultTemplate.BorderColor);
  WriteColor32(AConfigData, 'FillColor', FDefaultTemplate.FillColor);
  AConfigData.WriteInteger('LineWidth', FDefaultTemplate.LineWidth);
end;

function TMarkPolyTemplateConfig.GetDefaultTemplate: IMarkTemplatePoly;
begin
  LockRead;
  try
    Result := FDefaultTemplate;
  finally
    UnlockRead;
  end;
end;

procedure TMarkPolyTemplateConfig.SetDefaultTemplate(
  AValue: IMarkTemplatePoly);
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

