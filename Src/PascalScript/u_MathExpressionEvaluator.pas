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

unit u_MathExpressionEvaluator;

interface

uses
  SysUtils;

type
  TMathExpressionEvaluator = class
  private
    X, Y, Z: Integer;
    function TryParseExpression(var APos: PChar; out AValue: Integer): Boolean;
    function TryParseTerm(var APos: PChar; out AValue: Integer): Boolean;
    function TryParseNumber(var APos: PChar; out AValue: Integer): Boolean;
  public
    function Evaluate(const AExpression: string; const X, Y, Z: Integer): string;
  end;

implementation

{ TMathExpressionEvaluator }

function TMathExpressionEvaluator.Evaluate(const AExpression: string; const X, Y, Z: Integer): string;
var
  VPos: PChar;
  VValue: Integer;
begin
  if AExpression = '' then begin
    Assert(False, 'Empty expression!');
    Result := '';
    Exit;
  end;

  VPos := Pointer(AExpression);

  Self.X := X;
  Self.Y := Y;
  Self.Z := Z;

  if TryParseExpression(VPos, VValue) then begin
    Result := IntToStr(VValue);
  end else begin
    Assert(False, Format('Invalid expression: "%s" (x=%d, y=%d, z=%d)', [AExpression, X, Y, Z]));
    Result := '';
  end;
end;

function TMathExpressionEvaluator.TryParseExpression(var APos: PChar; out AValue: Integer): Boolean;
var
  VNumber: Integer;
begin
  Result := TryParseTerm(APos, AValue);
  if not Result then begin
    Exit;
  end;

  while CharInSet(APos^, ['+', '-']) do begin
    case APos^ of
      '+': begin
        Inc(APos);
        Result := TryParseTerm(APos, VNumber);
        if Result then begin
          Inc(AValue, VNumber);
        end else begin
          Exit;
        end;
      end;

      '-': begin
        Inc(APos);
        Result := TryParseTerm(APos, VNumber);
        if Result then begin
          Dec(AValue, VNumber);
        end else begin
          Exit;
        end;
      end;
    end;
  end;
end;

function TMathExpressionEvaluator.TryParseTerm(var APos: PChar; out AValue: Integer): Boolean;
var
  VNumber: Integer;
begin
  Result := TryParseNumber(APos, AValue);
  if not Result then begin
    Exit;
  end;

  while CharInSet(APos^, ['*', '/']) do begin
    case APos^ of
      '*': begin
        Inc(APos);
        Result := TryParseNumber(APos, VNumber);
        if Result then begin
          AValue := AValue * VNumber;
        end else begin
          Exit;
        end;
      end;

      '/': begin
        Inc(APos);
        Result := TryParseNumber(APos, VNumber) and (VNumber <> 0);
        if Result then begin
          AValue := AValue div VNumber; // integer division
        end else begin
          Exit;
        end;
      end;
    end;
  end;
end;

function TMathExpressionEvaluator.TryParseNumber(var APos: PChar; out AValue: Integer): Boolean;
var
  VStr: string;
  VStartPos: PChar;
begin
  // parse variables
  case APos^ of
    'x': begin
      AValue := X;
      Inc(APos);
      Exit(True);
    end;
    'y': begin
      AValue := Y;
      Inc(APos);
      Exit(True);
    end;
    'z': begin
      AValue := Z;
      Inc(APos);
      Exit(True);
    end;
  end;

  // parse number
  VStartPos := APos;

  while CharInSet(APos^, ['0'..'9']) do begin
    Inc(APos);
  end;

  if VStartPos = APos then begin
    Exit(False);
  end;

  VStr := '';
  SetString(VStr, VStartPos, APos - VStartPos);

  Result := TryStrToInt(VStr, AValue);
end;

end.
