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
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  protected
    function CreateTemplate(
      const ACategory: ICategory;
      ALineColor: TColor32;
      ALineWidth: Integer
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
  i_StringConfigDataElement,
  i_MarksDbSmlInternal,
  i_MarkCategoryFactoryDbInternal,
  u_StringConfigDataElementWithDefByStringRec,
  u_ConfigProviderHelpers,
  u_ResStrings,
  u_MarkTemplates;

{ TMarkLineTemplateConfig }

constructor TMarkLineTemplateConfig.Create(
  const ALanguageManager: ILanguageManager;
  const ACategoryDb: IMarkCategoryDBSmlInternal
);
var
  VFormatString: IStringConfigDataElement;
begin
  VFormatString :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager,
      True,
      'FormatString',
      True,
      @SAS_STR_NewPath
    );
  inherited Create(ACategoryDb, VFormatString);

  FDefaultTemplate :=
    CreateTemplate(
      nil,
      SetAlpha(clRed32, 166),
      2
    );
end;

function TMarkLineTemplateConfig.CreateTemplate(
  const ACategory: ICategory;
  ALineColor: TColor32;
  ALineWidth: Integer
): IMarkTemplateLine;
var
  VCategoryId: string;
begin
  VCategoryId := '';
  if ACategory <> nil then begin
    VCategoryId := ACategory.StringId;
  end;
  Result :=
    TMarkTemplateLine.Create(
      NameGenerator,
      VCategoryId,
      ALineColor,
      ALineWidth
    );
end;

procedure TMarkLineTemplateConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
var
  VCategoryId: string;
  VLineColor: TColor32;
  VLineWidth: Integer;
  VTemplate: IMarkTemplateLine;
begin
  inherited;
  VCategoryId := FDefaultTemplate.CategoryStringID;
  VLineColor := FDefaultTemplate.LineColor;
  VLineWidth := FDefaultTemplate.LineWidth;
  if AConfigData <> nil then begin
    VCategoryId := AConfigData.ReadString('CategoryId', VCategoryId);
    VLineColor := ReadColor32(AConfigData, 'LineColor', VLineColor);
    VLineWidth := AConfigData.ReadInteger('LineWidth', VLineWidth);
  end;
  VTemplate :=
    TMarkTemplateLine.Create(
      NameGenerator,
      VCategoryId,
      VLineColor,
      VLineWidth
    );

  SetDefaultTemplate(VTemplate);
end;

procedure TMarkLineTemplateConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteString('CategoryId', FDefaultTemplate.CategoryStringID);
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
