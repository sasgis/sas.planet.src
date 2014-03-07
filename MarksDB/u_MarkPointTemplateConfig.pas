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

unit u_MarkPointTemplateConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_Appearance,
  i_AppearanceOfMarkFactory,
  i_LanguageManager,
  i_MarkTemplate,
  i_Category,
  i_MarkFactoryConfig,
  u_MarkTemplateConfigBase;

type
  TMarkPointTemplateConfig = class(TMarkTemplateConfigBase, IMarkPointTemplateConfig)
  private
    FDefaultTemplate: IMarkTemplatePoint;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  protected
    function CreateTemplate(
      const AAppearance: IAppearance;
      const ACategory: ICategory
    ): IMarkTemplatePoint;

    function GetDefaultTemplate: IMarkTemplatePoint;
    procedure SetDefaultTemplate(const AValue: IMarkTemplatePoint);
  public
    constructor Create(
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const ALanguageManager: ILanguageManager
    );
  end;

implementation

uses
  i_StringConfigDataElement,
  u_ConfigProviderHelpers,
  u_StringConfigDataElementWithDefByStringRec,
  u_ResStrings,
  u_Category,
  u_MarkTemplates;

{ TMarkPointTemplateConfig }

constructor TMarkPointTemplateConfig.Create(
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
      @SAS_STR_NewMark
    );
  inherited Create(AAppearanceOfMarkFactory, VFormatString);

  VAppearance :=
    AppearanceOfMarkFactory.CreatePointAppearance(
      SetAlpha(clYellow32, 166),
      SetAlpha(clBlack32, 166),
      11,
      '',
      nil,
      32
    );
  FDefaultTemplate :=
    CreateTemplate(
      VAppearance,
      nil
    );
end;

function TMarkPointTemplateConfig.CreateTemplate(
  const AAppearance: IAppearance;
  const ACategory: ICategory
): IMarkTemplatePoint;
var
  VCategory: ICategory;
begin
  VCategory := ACategory;
  if VCategory = nil then begin
    VCategory := TCategory.Create('');
  end;

  Result :=
    TMarkTemplatePoint.Create(
      NameGenerator,
      VCategory,
      AAppearance
    );
end;

procedure TMarkPointTemplateConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
var
  VPicName: string;
  VCategoryName: string;
  VTextColor, VTextBgColor: TColor32;
  VFontSize, VMarkerSize: Integer;
  VTemplate: IMarkTemplatePoint;
  VAppearance: IAppearance;
begin
  inherited;
  VCategoryName := FDefaultTemplate.Category.Name;
  VTextColor := FDefaultTemplate.CaptionAppearance.TextColor;
  VTextBgColor := FDefaultTemplate.CaptionAppearance.TextBgColor;
  VFontSize := FDefaultTemplate.CaptionAppearance.FontSize;
  VMarkerSize := FDefaultTemplate.IconAppearance.MarkerSize;
  VPicName := FDefaultTemplate.IconAppearance.PicName;
  if AConfigData <> nil then begin
    VPicName := AConfigData.ReadString('IconName', VPicName);
    VCategoryName := AConfigData.ReadString('CategoryName', VCategoryName);
    VTextColor := ReadColor32(AConfigData, 'TextColor', VTextColor);
    VTextBgColor := ReadColor32(AConfigData, 'ShadowColor', VTextBgColor);
    VFontSize := AConfigData.ReadInteger('FontSize', VFontSize);
    VMarkerSize := AConfigData.ReadInteger('IconSize', VMarkerSize);
  end;
  VAppearance :=
    AppearanceOfMarkFactory.CreatePointAppearance(
      VTextColor,
      VTextBgColor,
      VFontSize,
      VPicName,
      nil,
      VMarkerSize
    );
  VTemplate :=
    CreateTemplate(
      VAppearance,
      TCategory.Create(VCategoryName)
    );
  SetDefaultTemplate(VTemplate);
end;

procedure TMarkPointTemplateConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteString('IconName', FDefaultTemplate.IconAppearance.PicName);
  AConfigData.WriteString('CategoryName', FDefaultTemplate.Category.Name);
  WriteColor32(AConfigData, 'TextColor', FDefaultTemplate.CaptionAppearance.TextColor);
  WriteColor32(AConfigData, 'ShadowColor', FDefaultTemplate.CaptionAppearance.TextBgColor);
  AConfigData.WriteInteger('FontSize', FDefaultTemplate.CaptionAppearance.FontSize);
  AConfigData.WriteInteger('IconSize', FDefaultTemplate.IconAppearance.MarkerSize);
end;

function TMarkPointTemplateConfig.GetDefaultTemplate: IMarkTemplatePoint;
begin
  LockRead;
  try
    Result := FDefaultTemplate;
  finally
    UnlockRead;
  end;
end;

procedure TMarkPointTemplateConfig.SetDefaultTemplate(
  const AValue: IMarkTemplatePoint
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
