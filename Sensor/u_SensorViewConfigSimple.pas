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

unit u_SensorViewConfigSimple;

interface

uses
  i_Sensor,
  u_ConfigDataElementBase;

type
  TSensorViewConfigSimple = class(TConfigDataElementBaseEmptySaveLoad, ISensorViewConfig)
  private
    FVisible: Boolean;
  private
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create;
  end;


implementation

{ TSensorViewConfigSimple }

constructor TSensorViewConfigSimple.Create;
begin
  inherited Create;
  FVisible := True;
end;

function TSensorViewConfigSimple.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

procedure TSensorViewConfigSimple.SetVisible(AValue: Boolean);
begin
  LockWrite;
  try
    if FVisible <> AValue then begin
      FVisible := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
