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

unit u_LastResponseInfo;

interface

uses
  SysUtils,
  i_LastResponseInfo,
  u_ChangeableBase;

type
  TLastResponseInfo = class(TChangeableWithSimpleLockBase, ILastResponseInfo)
  private
    FResponseHead: AnsiString;
  private
    function GetResponseHead: AnsiString;
    procedure SetResponseHead(const AValue: AnsiString);
  public
    constructor Create;
  end;

implementation

{ TLastResponseInfo }

constructor TLastResponseInfo.Create;
begin
  inherited Create;
end;

function TLastResponseInfo.GetResponseHead: AnsiString;
begin
  CS.BeginRead;
  try
    Result := FResponseHead;
  finally
    CS.EndRead;
  end;
end;

procedure TLastResponseInfo.SetResponseHead(const AValue: AnsiString);
var
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    if FResponseHead <> AValue then begin
      FResponseHead := AValue;
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

end.
