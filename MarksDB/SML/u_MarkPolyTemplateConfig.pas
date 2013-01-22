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
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  protected
    function CreateTemplate(
      const ACategory: ICategory;
      ABorderColor: TColor32;
      AFillColor: TColor32;
      ALineWidth: Integer
    ): IMarkTemplatePoly;

    function GetDefaultTemplate: IMarkTemplatePoly;
    procedure SetDefaultTemplate(const AValue: IMarkTemplatePoly);
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

{ TMarkPolyTemplateConfig }

constructor TMarkPolyTemplateConfig.Create(
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
      @SAS_STR_NewPoly
    );
  inherited Create(ACategoryDb, VFormatString);

  FDefaultTemplate :=
    CreateTemplate(
      nil,
      SetAlpha(clBlack32, 166),
      SetAlpha(clWhite32, 51),
      2
    );
end;

function TMarkPolyTemplateConfig.CreateTemplate(
  const ACategory: ICategory;
  ABorderColor: TColor32;
  AFillColor: TColor32;
  ALineWidth: Integer
): IMarkTemplatePoly;
var
  VCategoryId: string;
begin
  VCategoryId := '';
  if ACategory <> nil then begin
    VCategoryId := ACategory.StringId;
  end;
  Result :=
    TMarkTemplatePoly.Create(
      NameGenerator,
      VCategoryId,
      ABorderColor,
      AFillColor,
      ALineWidth
    );
end;

procedure TMarkPolyTemplateConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
var
  VBorderColor, VFillColor: TColor32;
  VLineWidth: Integer;
  VCategoryID: string;
  VTemplate: IMarkTemplatePoly;
begin
  inherited;
  VCategoryID := FDefaultTemplate.CategoryStringID;
  VBorderColor := FDefaultTemplate.BorderColor;
  VFillColor := FDefaultTemplate.FillColor;
  VLineWidth := FDefaultTemplate.LineWidth;
  if AConfigData <> nil then begin
    VCategoryID := AConfigData.ReadString('CategoryId', VCategoryID);
    VBorderColor := ReadColor32(AConfigData, 'LineColor', VBorderColor);
    VFillColor := ReadColor32(AConfigData, 'FillColor', VFillColor);
    VLineWidth := AConfigData.ReadInteger('LineWidth', VLineWidth);
  end;
  VTemplate :=
    TMarkTemplatePoly.Create(
      NameGenerator,
      VCategoryID,
      VBorderColor,
      VFillColor,
      VLineWidth
    );
  SetDefaultTemplate(VTemplate);
end;

procedure TMarkPolyTemplateConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteString('CategoryId', FDefaultTemplate.CategoryStringID);
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
  const AValue: IMarkTemplatePoly
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
