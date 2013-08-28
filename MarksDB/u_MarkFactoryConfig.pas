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

unit u_MarkFactoryConfig;

interface

uses
  i_LanguageManager,
  i_AppearanceOfMarkFactory,
  i_MarkFactoryConfig,
  u_ConfigDataElementComplexBase;

type
  TMarkFactoryConfig = class(TConfigDataElementComplexBase, IMarkFactoryConfig)
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
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const ALanguageManager: ILanguageManager
    );
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_MarkPointTemplateConfig,
  u_MarkLineTemplateConfig,
  u_MarkPolyTemplateConfig;

{ TMarkFactoryConfig }

constructor TMarkFactoryConfig.Create(
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const ALanguageManager: ILanguageManager
);
begin
  inherited Create;

  FPointTemplateConfig := TMarkPointTemplateConfig.Create(AAppearanceOfMarkFactory, ALanguageManager);
  Add(FPointTemplateConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarkNewPoint'));

  FLineTemplateConfig := TMarkLineTemplateConfig.Create(AAppearanceOfMarkFactory, ALanguageManager);
  Add(FLineTemplateConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarkNewLine'));

  FPolyTemplateConfig := TMarkPolyTemplateConfig.Create(AAppearanceOfMarkFactory, ALanguageManager);
  Add(FPolyTemplateConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarkNewPoly'));
end;

function TMarkFactoryConfig.GetLineTemplateConfig: IMarkLineTemplateConfig;
begin
  Result := FLineTemplateConfig;
end;

function TMarkFactoryConfig.GetPointTemplateConfig: IMarkPointTemplateConfig;
begin
  Result := FPointTemplateConfig;
end;

function TMarkFactoryConfig.GetPolyTemplateConfig: IMarkPolyTemplateConfig;
begin
  Result := FPolyTemplateConfig;
end;

end.
