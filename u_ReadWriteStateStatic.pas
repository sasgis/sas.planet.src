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

unit u_ReadWriteStateStatic;

interface

uses
  t_CommonTypes,
  i_ReadWriteState,
  u_BaseInterfacedObject;

type
  TReadWriteStateStatic = class(TBaseInterfacedObject, IReadWriteStateStatic)
  private
    FReadAccess: TAccesState;
    FWriteAccess: TAccesState;
  private
    function GetReadAccess: TAccesState;
    function GetWriteAccess: TAccesState;
  public
    constructor Create(
      AReadAccess: TAccesState;
      AWriteAccess: TAccesState
    );
  end;

implementation

{ TReadWriteStateStatic }

constructor TReadWriteStateStatic.Create(AReadAccess,
  AWriteAccess: TAccesState);
begin
  inherited Create;
  FReadAccess := AReadAccess;
  FWriteAccess := AWriteAccess;
end;

function TReadWriteStateStatic.GetReadAccess: TAccesState;
begin
  Result := FReadAccess;
end;

function TReadWriteStateStatic.GetWriteAccess: TAccesState;
begin
  Result := FWriteAccess;
end;

end.
