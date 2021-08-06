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
function RegExprGetMatchSubStr(const AStr, AMatchExpr: AnsiString; AMatchID: Integer): AnsiString;

{$region 'xmldoc'}
/// <summary>
/// Замена подстроки (поиск выполняется регулярным выражением)
/// </summary>
/// <param name="AStr">Исходная строка</param>
/// <param name="AMatchExpr">Рег. выражение, по которому выполняется поиск</param>
/// <param name="AReplace">Строка замены</param>
/// <returns>Исходная строка (если совпадений не найдено) или изменённая строка</returns>
{$endregion}
function RegExprReplaceMatchSubStr(const AStr, AMatchExpr, AReplace: AnsiString): AnsiString;

implementation

uses
  SysUtils,
  RegExpr;

function RegExprGetMatchSubStr(const AStr, AMatchExpr: AnsiString; AMatchID: Integer): AnsiString;
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

function RegExprReplaceMatchSubStr(const AStr, AMatchExpr, AReplace: AnsiString): AnsiString;
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

