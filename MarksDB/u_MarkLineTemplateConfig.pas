{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_MarkLineTemplateConfig;

interface

uses
  t_Bitmap32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_LanguageManager,
  i_Appearance,
  i_AppearanceOfMarkFactory,
  i_MarkTemplate,
  i_Category,
  i_MarkFactoryConfig,
  u_MarkTemplateConfigBase;

type
  TMarkLineTemplateConfig = class(TMarkTemplateConfigBase, IMarkLineTemplateConfig)
  private
    FDefaultTemplate: IMarkTemplateLine;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  protected
    function CreateTemplate(
      const AAppearance: IAppearance;
      const ACategory: ICategory
    ): IMarkTemplateLine;

    function GetDefaultTemplate: IMarkTemplateLine;
    procedure SetDefaultTemplate(const AValue: IMarkTemplateLine);
  public
    constructor Create(
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const ALanguageManager: ILanguageManager
    );
  end;

implementation

uses
  GR32,
  i_StringConfigDataElement,
  u_StringConfigDataElementWithDefByStringRec,
  u_ConfigProviderHelpers,
  u_Category,
  u_ResStrings,
  u_MarkTemplates;

{ TMarkLineTemplateConfig }

constructor TMarkLineTemplateConfig.Create(
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const ALanguageManager: ILanguageManager
);
var
  VFormatString: IStringConfigDataElement;
  VAppearance: IAppearance;
begin
  VFormatString :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager,
      True,
      'FormatString',
      True,
      @SAS_STR_NewPath
    );
  inherited Create(AAppearanceOfMarkFactory, VFormatString);
  VAppearance :=
    AppearanceOfMarkFactory.CreateLineAppearance(
      SetAlpha(clRed32, 166),
      2
    );
  FDefaultTemplate :=
    CreateTemplate(
      VAppearance,
      nil
    );
end;

function TMarkLineTemplateConfig.CreateTemplate(
  const AAppearance: IAppearance;
  const ACategory: ICategory
): IMarkTemplateLine;
var
  VCategory: ICategory;
begin
  VCategory := ACategory;
  if VCategory = nil then begin
    VCategory := TCategory.Create('');
  end;

  Result :=
    TMarkTemplateLine.Create(
      NameGenerator,
      VCategory,
      AAppearance
    );
end;

procedure TMarkLineTemplateConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
var
  VCategoryName: string;
  VLineColor: TColor32;
  VLineWidth: Integer;
  VTemplate: IMarkTemplateLine;
  VAppearance: IAppearance;
begin
  inherited;
  VCategoryName := FDefaultTemplate.Category.Name;
  VLineColor := FDefaultTemplate.LineAppearance.LineColor;
  VLineWidth := FDefaultTemplate.LineAppearance.LineWidth;
  if AConfigData <> nil then begin
    VCategoryName := AConfigData.ReadString('CategoryName', VCategoryName);
    VLineColor := ReadColor32(AConfigData, 'LineColor', VLineColor);
    VLineWidth := AConfigData.ReadInteger('LineWidth', VLineWidth);
  end;
  VAppearance :=
    AppearanceOfMarkFactory.CreateLineAppearance(
      VLineColor,
      VLineWidth
    );
  VTemplate :=
    CreateTemplate(
      VAppearance,
      TCategory.Create(VCategoryName)
    );

  SetDefaultTemplate(VTemplate);
end;

procedure TMarkLineTemplateConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteString('CategoryName', FDefaultTemplate.Category.Name);
  WriteColor32(AConfigData, 'LineColor', FDefaultTemplate.LineAppearance.LineColor);
  AConfigData.WriteInteger('LineWidth', FDefaultTemplate.LineAppearance.LineWidth);
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
