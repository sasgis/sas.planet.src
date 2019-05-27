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

unit u_StorageStateInternal;

interface

uses
  t_CommonTypes,
  i_TileStorageAbilities,
  i_StorageState,
  i_StorageStateInternal,
  u_ChangeableBase;

type
  TStorageStateInternal = class(TChangeableWithSimpleLockBase, IStorageStateInternal, IStorageStateChangeble)
  private
    FStorageForceAbilities: ITileStorageAbilities;

    FReadAccess: Boolean;
    FScanAccess: Boolean;

    FAddAccess: Boolean;
    FDeleteAccess: Boolean;
    FReplaceAccess: Boolean;

    FStatic: IStorageStateStatic;
    function CreateStatic: IStorageStateStatic; inline;
  private
    function GetReadAccess: Boolean;
    procedure SetReadAccess(AValue: Boolean);

    function GetScanAccess: Boolean;
    procedure SetScanAccess(AValue: Boolean);

    function GetAddAccess: Boolean;
    procedure SetAddAccess(AValue: Boolean);

    function GetDeleteAccess: Boolean;
    procedure SetDeleteAccess(AValue: Boolean);

    function GetReplaceAccess: Boolean;
    procedure SetReplaceAccess(AValue: Boolean);

    function GetStatic: IStorageStateStatic;
  public
    constructor Create(
      const AStorageForceAbilities: ITileStorageAbilities
    );
  end;

implementation

uses
  u_StorageStateStatic;

{ TStorageStateInternal }

constructor TStorageStateInternal.Create(
  const AStorageForceAbilities: ITileStorageAbilities
);
begin
  Assert(AStorageForceAbilities <> nil);
  inherited Create;

  FStorageForceAbilities := AStorageForceAbilities;

  FReadAccess := FStorageForceAbilities.AllowRead;
  FScanAccess := FStorageForceAbilities.AllowScan;

  FAddAccess := FStorageForceAbilities.AllowAdd;
  FDeleteAccess := FStorageForceAbilities.AllowDelete;
  FReplaceAccess := FStorageForceAbilities.AllowReplace;

  FStatic := CreateStatic;
end;

function TStorageStateInternal.CreateStatic: IStorageStateStatic;
begin
  Result :=
    TStorageStateStatic.Create(
      FReadAccess,
      FScanAccess,
      FAddAccess,
      FDeleteAccess,
      FReplaceAccess
    );
end;

function TStorageStateInternal.GetAddAccess: Boolean;
begin
  CS.BeginRead;
  try
    Result := FAddAccess;
  finally
    CS.EndRead;
  end;
end;

function TStorageStateInternal.GetDeleteAccess: Boolean;
begin
  CS.BeginRead;
  try
    Result := FDeleteAccess;
  finally
    CS.EndRead;
  end;
end;

function TStorageStateInternal.GetReadAccess: Boolean;
begin
  CS.BeginRead;
  try
    Result := FReadAccess;
  finally
    CS.EndRead;
  end;
end;

function TStorageStateInternal.GetReplaceAccess: Boolean;
begin
  CS.BeginRead;
  try
    Result := FReplaceAccess;
  finally
    CS.EndRead;
  end;
end;

function TStorageStateInternal.GetScanAccess: Boolean;
begin
  CS.BeginRead;
  try
    Result := FScanAccess;
  finally
    CS.EndRead;
  end;
end;

function TStorageStateInternal.GetStatic: IStorageStateStatic;
begin
  CS.BeginRead;
  try
    Result := FStatic;
  finally
    CS.EndRead;
  end;
end;

procedure TStorageStateInternal.SetReadAccess(AValue: Boolean);
var
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    if FReadAccess <> AValue then begin
      FReadAccess := AValue;
      VNeedNotify := True;
      FStatic := CreateStatic;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

procedure TStorageStateInternal.SetScanAccess(AValue: Boolean);
var
  VValue: Boolean;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := AValue and FStorageForceAbilities.AllowScan;

  CS.BeginWrite;
  try
    if FScanAccess <> VValue then begin
      FScanAccess := VValue;
      VNeedNotify := True;
      FStatic := CreateStatic;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

procedure TStorageStateInternal.SetAddAccess(AValue: Boolean);
var
  VValue: Boolean;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := AValue and FStorageForceAbilities.AllowAdd;

  CS.BeginWrite;
  try
    if FAddAccess <> VValue then begin
      FAddAccess := VValue;
      VNeedNotify := True;
      FStatic := CreateStatic;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

procedure TStorageStateInternal.SetDeleteAccess(AValue: Boolean);
var
  VValue: Boolean;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := AValue and FStorageForceAbilities.AllowDelete;

  CS.BeginWrite;
  try
    if FDeleteAccess <> VValue then begin
      FDeleteAccess := VValue;
      VNeedNotify := True;
      FStatic := CreateStatic;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

procedure TStorageStateInternal.SetReplaceAccess(AValue: Boolean);
var
  VValue: Boolean;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := AValue and FStorageForceAbilities.AllowReplace;

  CS.BeginWrite;
  try
    if FReplaceAccess <> VValue then begin
      FReplaceAccess := VValue;
      VNeedNotify := True;
      FStatic := CreateStatic;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

end.
