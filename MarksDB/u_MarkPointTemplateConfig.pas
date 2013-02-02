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

unit u_MarkPointTemplateConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_LanguageManager,
  i_MarkPicture,
  i_MarkTemplate,
  i_MarkCategory,
  i_MarksFactoryConfig,
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
      const APicName: string;
      const ACategory: ICategory;
      ATextColor, ATextBgColor: TColor32;
      AFontSize, AMarkerSize: Integer
    ): IMarkTemplatePoint;

    function GetDefaultTemplate: IMarkTemplatePoint;
    procedure SetDefaultTemplate(const AValue: IMarkTemplatePoint);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager
    );
  end;

implementation

uses
  i_StringConfigDataElement,
  u_ConfigProviderHelpers,
  u_StringConfigDataElementWithDefByStringRec,
  u_ResStrings,
  u_MarkTemplates;

{ TMarkPointTemplateConfig }

constructor TMarkPointTemplateConfig.Create(
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
      @SAS_STR_NewMark
    );
  inherited Create(VFormatString);

  FDefaultTemplate :=
    CreateTemplate(
      '',
      nil,
      SetAlpha(clYellow32, 166),
      SetAlpha(clBlack32, 166),
      11,
      32
    );
end;

function TMarkPointTemplateConfig.CreateTemplate(
  const APicName: string;
  const ACategory: ICategory;
  ATextColor, ATextBgColor: TColor32;
  AFontSize, AMarkerSize: Integer
): IMarkTemplatePoint;
var
  VCategoryStringId: string;
begin
  VCategoryStringId := '';
  if ACategory <> nil then begin
    VCategoryStringId := ACategory.StringId;
  end;

  Result :=
    TMarkTemplatePoint.Create(
      NameGenerator,
      VCategoryStringId,
      ATextColor,
      ATextBgColor,
      AFontSize,
      AMarkerSize,
      APicName
    );
end;

procedure TMarkPointTemplateConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
var
  VPicName: string;
  VCategoryId: string;
  VTextColor, VTextBgColor: TColor32;
  VFontSize, VMarkerSize: Integer;
  VTemplate: IMarkTemplatePoint;
begin
  inherited;
  VCategoryId := FDefaultTemplate.CategoryStringID;
  VTextColor := FDefaultTemplate.TextColor;
  VTextBgColor := FDefaultTemplate.TextBgColor;
  VFontSize := FDefaultTemplate.FontSize;
  VMarkerSize := FDefaultTemplate.MarkerSize;
  VPicName := FDefaultTemplate.PicName;
  if AConfigData <> nil then begin
    VPicName := AConfigData.ReadString('IconName', VPicName);
    VCategoryId := AConfigData.ReadString('CategoryId', VCategoryId);
    VTextColor := ReadColor32(AConfigData, 'TextColor', VTextColor);
    VTextBgColor := ReadColor32(AConfigData, 'ShadowColor', VTextBgColor);
    VFontSize := AConfigData.ReadInteger('FontSize', VFontSize);
    VMarkerSize := AConfigData.ReadInteger('IconSize', VMarkerSize);
  end;
  VTemplate :=
    TMarkTemplatePoint.Create(
      NameGenerator,
      VCategoryId,
      VTextColor,
      VTextBgColor,
      VFontSize,
      VMarkerSize,
      VPicName
    );
  SetDefaultTemplate(VTemplate);
end;

procedure TMarkPointTemplateConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteString('IconName', FDefaultTemplate.PicName);
  AConfigData.WriteString('CategoryId', FDefaultTemplate.CategoryStringID);
  WriteColor32(AConfigData, 'TextColor', FDefaultTemplate.TextColor);
  WriteColor32(AConfigData, 'ShadowColor', FDefaultTemplate.TextBgColor);
  AConfigData.WriteInteger('FontSize', FDefaultTemplate.FontSize);
  AConfigData.WriteInteger('IconSize', FDefaultTemplate.MarkerSize);
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
