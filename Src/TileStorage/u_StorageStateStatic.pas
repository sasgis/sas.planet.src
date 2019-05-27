{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_StorageStateStatic;

interface

uses
  i_StorageState,
  u_BaseInterfacedObject;

type
  TStorageStateStatic = class(TBaseInterfacedObject, IStorageStateStatic)
  private
    FReadAccess: Boolean;
    FScanAccess: Boolean;
    FAddAccess: Boolean;
    FDeleteAccess: Boolean;
    FReplaceAccess: Boolean;
  private
    { IStorageStateStatic }
    function GetReadAccess: Boolean;
    function GetScanAccess: Boolean;
    function GetAddAccess: Boolean;
    function GetDeleteAccess: Boolean;
    function GetReplaceAccess: Boolean;
    function IsSame(const AValue: IStorageStateStatic): Boolean;
  public
    constructor Create(
      AReadAccess: Boolean;
      AScanAccess: Boolean;
      AAddAccess: Boolean;
      ADeleteAccess: Boolean;
      AReplaceAccess: Boolean
    );
  end;

implementation

{ TStorageStateStatic }

constructor TStorageStateStatic.Create(
  AReadAccess: Boolean;
  AScanAccess: Boolean;
  AAddAccess: Boolean;
  ADeleteAccess: Boolean;
  AReplaceAccess: Boolean
);
begin
  inherited Create;
  FReadAccess := AReadAccess;
  FScanAccess := AScanAccess;
  FAddAccess := AAddAccess;
  FDeleteAccess := ADeleteAccess;
  FReplaceAccess := AReplaceAccess;
end;


function TStorageStateStatic.GetAddAccess: Boolean;
begin
  Result := FAddAccess;
end;

function TStorageStateStatic.GetDeleteAccess: Boolean;
begin
  Result := FDeleteAccess;
end;

function TStorageStateStatic.GetReadAccess: Boolean;
begin
  Result := FReadAccess;
end;

function TStorageStateStatic.GetReplaceAccess: Boolean;
begin
  Result := FReplaceAccess;
end;

function TStorageStateStatic.GetScanAccess: Boolean;
begin
  Result := FScanAccess;
end;

function TStorageStateStatic.IsSame(const AValue: IStorageStateStatic): Boolean;
begin
  if AValue = nil then begin
    Result := False;
  end else if AValue = IStorageStateStatic(Self) then begin
    Result := True;
  end else begin
    Result :=
      (AValue.ReadAccess = FReadAccess) and
      (AValue.ScanAccess = FScanAccess) and
      (AValue.AddAccess = FAddAccess) and
      (AValue.DeleteAccess = FDeleteAccess) and
      (AValue.ReplaceAccess = FReplaceAccess);
  end;
end;

end.
