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

unit RegExprUtils;

interface

{$region 'xmldoc'}
/// <summary>
/// Поиск подстроки регулярным выражением
/// </summary>
/// <param name="AStr">Исходная строка</param>
/// <param name="AMatchExpr">Рег. выражение, по которому выполняется поиск</param>
/// <param name="AMatchID">Номер совпадения рег. выражения (0,1,2 и т.д.)</param>
/// <returns>Искомая строка или пустая строка, если совпадений не найдено</returns>
{$endregion}
function RegExprGetMatchSubStr(const AStr, AMatchExpr: string; AMatchID: Integer): string;

{$region 'xmldoc'}
/// <summary>
/// Замена подстроки (поиск выполняется регулярным выражением)
/// </summary>
/// <param name="AStr">Исходная строка</param>
/// <param name="AMatchExpr">Рег. выражение, по которому выполняется поиск</param>
/// <param name="AReplace">Строка замены</param>
/// <returns>Исходная строка (если совпадений не найдено) или изменённая строка</returns>
{$endregion}
function RegExprReplaceMatchSubStr(const AStr, AMatchExpr, AReplace: string): string;

implementation

uses
  SysUtils,
  RegExpr;

function RegExprGetMatchSubStr(const AStr, AMatchExpr: string; AMatchID: Integer): string;
var
  VRegExpr: TRegExpr;
begin
    VRegExpr  := TRegExpr.Create;
  try
    VRegExpr.Expression := AMatchExpr;
    if VRegExpr.Exec(AStr) then
    begin
      if (AMatchID <= VRegExpr.SubExprMatchCount) and (AMatchID >= 0) then
        Result := VRegExpr.Match[AMatchID]
      else
        Result := '';
    end
    else
      Result := '';
  finally
    FreeAndNil(VRegExpr);
  end;
end;

function RegExprReplaceMatchSubStr(const AStr, AMatchExpr, AReplace: string): string;
var
  VRegExpr: TRegExpr;
begin
    VRegExpr  := TRegExpr.Create;
  try
    VRegExpr.Expression := AMatchExpr;
    if VRegExpr.Exec(AStr) then
      Result := VRegExpr.Replace(AStr, AReplace, True)
    else
      Result := AStr;
  finally
    FreeAndNil(VRegExpr);
  end;
end;

end.

