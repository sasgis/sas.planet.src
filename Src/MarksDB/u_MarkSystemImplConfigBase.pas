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

unit u_MarkSystemImplConfigBase;

interface

uses
  i_MarkSystemImplConfig,
  u_BaseInterfacedObject;

type
  TMarkSystemImplConfigBase = class(TBaseInterfacedObject, IMarkSystemImplConfigStatic)
  protected
    FFileName: string;
    FIsReadOnly: Boolean;
  protected
    function GetFileName: string;
    function GetIsReadOnly: Boolean;
  public
    constructor Create(
      const AFileName: string;
      const AIsReadOnly: Boolean
    );
  end;

implementation

{ TMarkSystemImplConfigBase }

constructor TMarkSystemImplConfigBase.Create(
  const AFileName: string;
  const AIsReadOnly: Boolean
);
begin
  inherited Create;
  FFileName := AFileName;
  FIsReadOnly := AIsReadOnly;
end;

function TMarkSystemImplConfigBase.GetFileName: string;
begin
  Result := FFileName;
end;

function TMarkSystemImplConfigBase.GetIsReadOnly: Boolean;
begin
  Result := FIsReadOnly;
end;

end.
