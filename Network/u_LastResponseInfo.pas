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

unit u_LastResponseInfo;

interface

uses
  i_LastResponseInfo,
  u_ConfigDataElementBase;

type
  TLastResponseInfo = class(TConfigDataElementBaseEmptySaveLoad, ILastResponseInfo)
  private
    FResponseHead: AnsiString;
  private
    function GetResponseHead: AnsiString;
    procedure SetResponseHead(const AValue: AnsiString);
  end;

implementation

{ TLastResponseInfo }

function TLastResponseInfo.GetResponseHead: AnsiString;
begin
  LockRead;
  try
    Result := FResponseHead;
  finally
    UnlockRead;
  end;
end;

procedure TLastResponseInfo.SetResponseHead(const AValue: AnsiString);
begin
  LockWrite;
  try
    if FResponseHead <> AValue then begin
      FResponseHead := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
