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

unit u_MarksFactoryConfig;

interface

uses
  i_LanguageManager,
  i_MarkPicture,
  i_MarksFactoryConfig,
  i_MarkCategoryDBSmlInternal,
  u_ConfigDataElementComplexBase;

type
  TMarksFactoryConfig = class(TConfigDataElementComplexBase, IMarksFactoryConfig)
  private
    FPointTemplateConfig: IMarkPointTemplateConfig;
    FLineTemplateConfig: IMarkLineTemplateConfig;
    FPolyTemplateConfig: IMarkPolyTemplateConfig;
  private
    function GetPointTemplateConfig: IMarkPointTemplateConfig;
    function GetLineTemplateConfig: IMarkLineTemplateConfig;
    function GetPolyTemplateConfig: IMarkPolyTemplateConfig;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ACategoryDb: IMarkCategoryDBSmlInternal;
      const AMarkPictureList: IMarkPictureList
    );
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_MarkPointTemplateConfig,
  u_MarkLineTemplateConfig,
  u_MarkPolyTemplateConfig;

{ TMarksFactoryConfig }

constructor TMarksFactoryConfig.Create(
  const ALanguageManager: ILanguageManager;
  const ACategoryDb: IMarkCategoryDBSmlInternal;
  const AMarkPictureList: IMarkPictureList
);
begin
  inherited Create;

  FPointTemplateConfig := TMarkPointTemplateConfig.Create(ALanguageManager, ACategoryDb, AMarkPictureList);
  Add(FPointTemplateConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarkNewPoint'));

  FLineTemplateConfig := TMarkLineTemplateConfig.Create(ALanguageManager, ACategoryDb);
  Add(FLineTemplateConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarkNewLine'));

  FPolyTemplateConfig := TMarkPolyTemplateConfig.Create(ALanguageManager, ACategoryDb);
  Add(FPolyTemplateConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarkNewPoly'));
end;

function TMarksFactoryConfig.GetLineTemplateConfig: IMarkLineTemplateConfig;
begin
  Result := FLineTemplateConfig;
end;

function TMarksFactoryConfig.GetPointTemplateConfig: IMarkPointTemplateConfig;
begin
  Result := FPointTemplateConfig;
end;

function TMarksFactoryConfig.GetPolyTemplateConfig: IMarkPolyTemplateConfig;
begin
  Result := FPolyTemplateConfig;
end;

end.
