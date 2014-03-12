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

unit u_StorageStateStatic;

interface

uses
  t_CommonTypes,
  i_StorageState,
  u_BaseInterfacedObject;

type
  TStorageStateStatic = class(TBaseInterfacedObject, IStorageStateStatic)
  private
    FReadAccess: TAccesState;
    FWriteAccess: TAccesState;
    FDeleteAccess: TAccesState;
    FAddAccess: TAccesState;
    FReplaceAccess: TAccesState;
  private
    function GetReadAccess: TAccesState;
    function GetWriteAccess: TAccesState;
    function GetDeleteAccess: TAccesState;
    function GetAddAccess: TAccesState;
    function GetReplaceAccess: TAccesState;
    function IsSame(const AValue: IStorageStateStatic): Boolean;
  public
    constructor Create(
      AReadAccess: TAccesState;
      AWriteAccess: TAccesState;
      ADeleteAccess: TAccesState;
      AAddAccess: TAccesState;
      AReplaceAccess: TAccesState
    );
  end;

implementation

{ TStorageStateStatic }

constructor TStorageStateStatic.Create(
  AReadAccess: TAccesState;
  AWriteAccess: TAccesState;
  ADeleteAccess: TAccesState;
  AAddAccess: TAccesState;
  AReplaceAccess: TAccesState
);
begin
  inherited Create;
  FReadAccess := AReadAccess;
  FWriteAccess := AWriteAccess;
  FDeleteAccess := ADeleteAccess;
  FAddAccess := AAddAccess;
  FReplaceAccess := AReplaceAccess;
end;


function TStorageStateStatic.GetAddAccess: TAccesState;
begin
  Result := FAddAccess;
end;

function TStorageStateStatic.GetDeleteAccess: TAccesState;
begin
  Result := FDeleteAccess;
end;

function TStorageStateStatic.GetReadAccess: TAccesState;
begin
  Result := FReadAccess;
end;

function TStorageStateStatic.GetReplaceAccess: TAccesState;
begin
  Result := FReplaceAccess;
end;

function TStorageStateStatic.GetWriteAccess: TAccesState;
begin
  Result := FWriteAccess;
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
      (AValue.WriteAccess = FWriteAccess) and
      (AValue.DeleteAccess = FDeleteAccess) and
      (AValue.AddAccess = FAddAccess) and
      (AValue.ReplaceAccess = FReplaceAccess);
  end;
end;

end.
