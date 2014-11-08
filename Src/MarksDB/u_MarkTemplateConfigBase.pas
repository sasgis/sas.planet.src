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
