{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_MarkTemplateConfigBase;

interface

uses
  i_MarkNameGenerator,
  i_AppearanceOfMarkFactory,
  i_StringConfigDataElement,
  u_ConfigDataElementComplexBase;

type
  TMarkTemplateConfigBase = class(TConfigDataElementComplexBase)
  private
    FNameGenerator: IMarkNameGenerator;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  protected
    property NameGenerator: IMarkNameGenerator read FNameGenerator;
    property AppearanceOfMarkFactory: IAppearanceOfMarkFactory read FAppearanceOfMarkFactory;
  protected
    function GetNameGenerator: IMarkNameGenerator;
  public
    constructor Create(
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AFormatString: IStringConfigDataElement
    );
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_MarkNameGenerator;

{ TMarkTemplateConfigBase }

constructor TMarkTemplateConfigBase.Create(
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AFormatString: IStringConfigDataElement
);
begin
  inherited Create;

  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;

  FNameGenerator := TMarkNameGenerator.Create(AFormatString);
  Add(FNameGenerator, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Name'), False, False, False, False);
end;

function TMarkTemplateConfigBase.GetNameGenerator: IMarkNameGenerator;
begin
  Result := FNameGenerator;
end;

end.
