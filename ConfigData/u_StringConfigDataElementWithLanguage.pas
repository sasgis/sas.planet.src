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
