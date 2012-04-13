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
  i_MarkCategoryDBSmlInternal,
  u_MarkTemplateConfigBase;

type
  TMarkPointTemplateConfig = class(TMarkTemplateConfigBase, IMarkPointTemplateConfig)
  private
    FDefaultTemplate: IMarkTemplatePoint;
    FMarkPictureList: IMarkPictureList;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function CreateTemplate(
      APic: IMarkPicture;
      ACategory: ICategory;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer
    ): IMarkTemplatePoint;

    function GetMarkPictureList: IMarkPictureList;

    function GetDefaultTemplate: IMarkTemplatePoint;
    procedure SetDefaultTemplate(AValue: IMarkTemplatePoint);
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      ACategoryDb: IMarkCategoryDBSmlInternal;
      AMarkPictureList: IMarkPictureList
    );
  end;

implementation

uses
  SysUtils,
  i_MarksDbSmlInternal,
  u_ConfigProviderHelpers,
  u_StringConfigDataElementWithDefByStringRec,
  u_ResStrings,
  u_MarkTemplates;

{ TMarkPointTemplateConfig }

constructor TMarkPointTemplateConfig.Create(
  ALanguageManager: ILanguageManager;
  ACategoryDb: IMarkCategoryDBSmlInternal;
  AMarkPictureList: IMarkPictureList
);
var
  VPic: IMarkPicture;
begin
  inherited Create(
    ACategoryDb,
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager,
      @SAS_STR_NewMark,
      True,
      'FormatString',
      True
    )
  );

  FMarkPictureList := AMarkPictureList;
  if FMarkPictureList.Count > 0 then begin
    VPic := FMarkPictureList.Get(0);
  end else begin
    VPic := nil;
  end;

  FDefaultTemplate := CreateTemplate(
    VPic,
    nil,
    SetAlpha(clYellow32, 166),
    SetAlpha(clBlack32, 166),
    11,
    32
  );
end;

function TMarkPointTemplateConfig.CreateTemplate(
  APic: IMarkPicture;
  ACategory: ICategory;
  AColor1, AColor2: TColor32;
  AScale1, AScale2: Integer
): IMarkTemplatePoint;
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
  Result := TMarkTemplatePoint.Create(
    CategoryDb,
    NameGenerator,
    VCategoryId,
    AColor1,
    AColor2,
    AScale1,
    AScale2,
    APic
  );
end;

procedure TMarkPointTemplateConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
var
  VPicName: string;
  VPic: IMarkPicture;
  VPicIndex: Integer;
  VCategoryId: Integer;
  VTextColor, VTextBgColor: TColor32;
  VFontSize, VMarkerSize: Integer;
  VTemplateInternal: IMarkTemplateSMLInternal;
begin
  inherited;
  VCategoryID := -1;
  if Supports(FDefaultTemplate, IMarkTemplateSMLInternal, VTemplateInternal) then begin
    VCategoryId := VTemplateInternal.CategoryId;
  end;
  VTextColor := FDefaultTemplate.TextColor;
  VTextBgColor := FDefaultTemplate.TextBgColor;
  VFontSize := FDefaultTemplate.FontSize;
  VMarkerSize := FDefaultTemplate.MarkerSize;
  VPic := FDefaultTemplate.Pic;
  if VPic = nil then begin
    VPic := FMarkPictureList.GetDefaultPicture;
  end;
  if VPic <> nil then begin
    VPicName := VPic.GetName;
  end;
  if AConfigData <> nil then begin
    VPicName := AConfigData.ReadString('IconName', VPicName);
    if VPicName = '' then begin
      VPic := nil;
    end else begin
      VPicIndex := FMarkPictureList.GetIndexByName(VPicName);
      if VPicIndex < 0 then begin
        VPic := nil;
      end else begin
        VPic := FMarkPictureList.Get(VPicIndex);
      end;
    end;
    VCategoryId := AConfigData.ReadInteger('CategoryId', VCategoryId);
    VTextColor := ReadColor32(AConfigData, 'TextColor', VTextColor);
    VTextBgColor := ReadColor32(AConfigData, 'ShadowColor', VTextBgColor);
    VFontSize := AConfigData.ReadInteger('FontSize', VFontSize);
    VMarkerSize := AConfigData.ReadInteger('IconSize', VMarkerSize);
  end;
  SetDefaultTemplate(
    TMarkTemplatePoint.Create(
      CategoryDb,
      NameGenerator,
      VCategoryId,
      VTextColor,
      VTextBgColor,
      VFontSize,
      VMarkerSize,
      VPic
    )
  );
end;

procedure TMarkPointTemplateConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
var
  VCategoryId: Integer;
  VPicName: string;
  VTemplateInternal: IMarkTemplateSMLInternal;
  VPic: IMarkPicture;
begin
  inherited;
  VCategoryID := -1;
  if Supports(FDefaultTemplate, IMarkTemplateSMLInternal, VTemplateInternal) then begin
    VCategoryId := VTemplateInternal.CategoryId;
  end;
  VPicName := '';
  VPic := FDefaultTemplate.Pic;
  if VPic <> nil then begin
    VPicName := VPic.GetName;
  end;
  AConfigData.WriteString('IconName', VPicName);
  AConfigData.WriteInteger('CategoryId', VCategoryId);
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

function TMarkPointTemplateConfig.GetMarkPictureList: IMarkPictureList;
begin
  Result := FMarkPictureList;
end;

procedure TMarkPointTemplateConfig.SetDefaultTemplate(
  AValue: IMarkTemplatePoint);
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
