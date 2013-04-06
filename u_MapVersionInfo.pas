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

unit u_MapVersionInfo;

interface

uses
  i_MapVersionInfo,
  u_BaseInterfacedObject;

type
  TMapVersionInfo = class(TBaseInterfacedObject, IMapVersionInfo)
  private
    FVersion: string;
    FShowPrevVersion: Boolean;
  private
    function GetUrlString: string;
    function GetStoreString: string;
    function GetCaption: string;
    function GetShowPrevVersion: Boolean;

    //function IsSame(const AValue: IMapVersionInfo): Boolean;
  public
    constructor Create(
      const AVersion: string;
      const AShowPrevVersion: Boolean
    );
  end;

implementation

{ TMapVersionInfo }

constructor TMapVersionInfo.Create(
  const AVersion: string;
  const AShowPrevVersion: Boolean
);
begin
  inherited Create;
  FVersion := AVersion;
  FShowPrevVersion := AShowPrevVersion;
end;

function TMapVersionInfo.GetCaption: string;
begin
  Result := FVersion;
end;

function TMapVersionInfo.GetShowPrevVersion: Boolean;
begin
  Result := FShowPrevVersion;
end;

function TMapVersionInfo.GetStoreString: string;
begin
  Result := FVersion;
end;

function TMapVersionInfo.GetUrlString: string;
begin
  Result := FVersion;
end;

(*
function TMapVersionInfo.IsSame(const AValue: IMapVersionInfo): Boolean;
begin
  // not used!
  if AValue = nil then begin
    Result := False;
  end else begin
    if AValue = IMapVersionInfo(Self) then begin
      Result := True;
    end else begin
      // do not check ShowPrevVersion!
      Result := AValue.StoreString = FVersion;
    end;
  end;
end;
*)

end.
