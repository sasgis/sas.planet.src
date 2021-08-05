{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit u_StringConfigDataElementWithLanguage;

interface

uses
  i_StringByLanguage,
  i_LanguageManager,
  u_StringConfigDataElementWithDefBase;

type
  TStringConfigDataElementWithLanguage = class(TStringConfigDataElementWithDefBase)
  private
    FDefValuesByLanguage: IStringByLanguage;
  protected
    function GetDefValueForCurrentLang: string; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      AUseSotre: Boolean;
      const AStoreIdentifier: string;
      AIsStoreDefault: Boolean;
      const ADefValuesByLanguage: IStringByLanguage
    );
  end;

implementation

{ TStringConfigDataElementWithLanguage }

constructor TStringConfigDataElementWithLanguage.Create(
  const ALanguageManager: ILanguageManager;
  AUseSotre: Boolean;
  const AStoreIdentifier: string;
  AIsStoreDefault: Boolean;
  const ADefValuesByLanguage: IStringByLanguage
);
begin
  inherited Create(ALanguageManager, AUseSotre, AStoreIdentifier, AIsStoreDefault);
  FDefValuesByLanguage := ADefValuesByLanguage;
end;

function TStringConfigDataElementWithLanguage.GetDefValueForCurrentLang: string;
var
  VIndex: Integer;
begin
  VIndex := LanguageManager.CurrentLanguageIndex;
  Result := FDefValuesByLanguage.GetString(VIndex);
end;

end.
