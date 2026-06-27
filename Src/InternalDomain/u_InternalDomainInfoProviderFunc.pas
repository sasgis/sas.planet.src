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

unit u_InternalDomainInfoProviderFunc;

interface

type
  TInternalDomainInfoProviderFunc = record
    class function ParseUrl(const AUrl: string; out ADomain, AFilePath: string): Boolean; static;
  end;

implementation

uses
  StrUtils;

class function TInternalDomainInfoProviderFunc.ParseUrl(
  const AUrl: string;
  out ADomain, AFilePath: string
): Boolean;
var
  VProtoclSeparator: string;
  VFileNameSeparator: string;
  VPos: Integer;
  VDomainWithFileName: string;
begin
  Result := False;
  ADomain := '';
  AFilePath := '';
  VProtoclSeparator := '://';
  VFileNameSeparator := '/';
  VPos := Pos(VProtoclSeparator, AUrl);
  if VPos > 0 then begin
    VDomainWithFileName := RightStr(AUrl, Length(AUrl) - VPos - Length(VProtoclSeparator) + 1);
    if Length(VDomainWithFileName) > 0 then begin
      VPos := Pos(VFileNameSeparator, VDomainWithFileName);
      if VPos > 0 then begin
        ADomain := LeftStr(VDomainWithFileName, VPos - 1);
        AFilePath := RightStr(VDomainWithFileName, Length(VDomainWithFileName) - VPos - Length(VFileNameSeparator) + 1);
      end else begin
        ADomain := VDomainWithFileName;
      end;
      if Length(ADomain) > 0 then begin
        Result := True;
      end;
    end;
  end;
end;

end.
