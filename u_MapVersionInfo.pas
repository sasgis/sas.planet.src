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

unit u_MapVersionInfo;

interface

uses
  t_Hash,
  i_MapVersionInfo,
  u_BaseInterfacedObject;

type
  TMapVersionInfo = class(TBaseInterfacedObject, IMapVersionInfo)
  private
    FHash: THashValue;
    FVersion: string;
  private
    function GetHash: THashValue;
    function GetUrlString: string;
    function GetStoreString: string;
    function GetCaption: string;
    function IsSame(const AValue: IMapVersionInfo): Boolean;
  public
    constructor Create(
      const AHash: THashValue;
      const AVersion: string
    );
  end;

implementation

{ TMapVersionInfo }

constructor TMapVersionInfo.Create(
  const AHash: THashValue;
  const AVersion: string
);
begin
  inherited Create;
  FHash := AHash;
  FVersion := AVersion;
end;

function TMapVersionInfo.GetCaption: string;
begin
  Result := FVersion;
end;

function TMapVersionInfo.GetHash: THashValue;
begin
  Result := FHash;
end;

function TMapVersionInfo.GetStoreString: string;
begin
  Result := FVersion;
end;

function TMapVersionInfo.GetUrlString: string;
begin
  Result := FVersion;
end;

function TMapVersionInfo.IsSame(const AValue: IMapVersionInfo): Boolean;
begin
  if AValue = nil then begin
    Result := False;
  end else begin
    if AValue = IMapVersionInfo(Self) then begin
      Result := True;
    end else begin
      if (FHash <> 0) and (AValue.Hash <> 0) and (FHash <> AValue.Hash) then begin
        Result := False;
      end else begin
        Result := AValue.StoreString = FVersion;
      end;
    end;
  end;
end;

end.
