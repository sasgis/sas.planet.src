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
  i_Category,
  i_MarksFactoryConfig,
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
      const ALanguageManager: ILanguageManager
    );
  end;

implementation

uses
  i_StringConfigDataElement,
  u_StringConfigDataElementWithDefByStringRec,
  u_ConfigProviderHelpers,
  u_ResStrings,
  u_Category,
  u_MarkTemplates;

{ TMarkPolyTemplateConfig }

constructor TMarkPolyTemplateConfig.Create(
  const ALanguageManager: ILanguageManager
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
  inherited Create(VFormatString);

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
  VCategory: ICategory;
begin
  VCategory := ACategory;
  if VCategory = nil then begin
    VCategory := TCategory.Create('');
  end;

  Result :=
    TMarkTemplatePoly.Create(
      NameGenerator,
      VCategory,
      ABorderColor,
      AFillColor,
      ALineWidth
    );
end;

procedure TMarkPolyTemplateConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
var
  VLineColor, VFillColor: TColor32;
  VLineWidth: Integer;
  VCategoryName: string;
  VTemplate: IMarkTemplatePoly;
begin
  inherited;
  VCategoryName := FDefaultTemplate.Category.Name;
  VLineColor := FDefaultTemplate.LineColor;
  VFillColor := FDefaultTemplate.FillColor;
  VLineWidth := FDefaultTemplate.LineWidth;
  if AConfigData <> nil then begin
    VCategoryName := AConfigData.ReadString('CategoryName', VCategoryName);
    VLineColor := ReadColor32(AConfigData, 'LineColor', VLineColor);
    VFillColor := ReadColor32(AConfigData, 'FillColor', VFillColor);
    VLineWidth := AConfigData.ReadInteger('LineWidth', VLineWidth);
  end;
  VTemplate :=
    TMarkTemplatePoly.Create(
      NameGenerator,
      TCategory.Create(VCategoryName),
      VLineColor,
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
  AConfigData.WriteString('CategoryName', FDefaultTemplate.Category.Name);
  WriteColor32(AConfigData, 'LineColor', FDefaultTemplate.LineColor);
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
