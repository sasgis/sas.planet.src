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

    FReadAccess: TAccesState;
    FWriteAccess: TAccesState;
    FDeleteAccess: TAccesState;
    FAddAccess: TAccesState;
    FReplaceAccess: TAccesState;

    FStatic: IStorageStateStatic;
    function CreateStatic: IStorageStateStatic;
  private
    function GetReadAccess: TAccesState;
    procedure SetReadAccess(AValue: TAccesState);

    function GetWriteAccess: TAccesState;
    procedure SetWriteAccess(AValue: TAccesState);

    function GetDeleteAccess: TAccesState;
    procedure SetDeleteAccess(AValue: TAccesState);

    function GetAddAccess: TAccesState;
    procedure SetAddAccess(AValue: TAccesState);

    function GetReplaceAccess: TAccesState;
    procedure SetReplaceAccess(AValue: TAccesState);

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

  FReadAccess := asUnknown;
  FWriteAccess := asUnknown;
  FDeleteAccess := asUnknown;
  FAddAccess := asUnknown;
  FReplaceAccess := asUnknown;
  if not FStorageForceAbilities.AllowRead then begin
    FReadAccess := asDisabled;
  end;

  if FStorageForceAbilities.IsReadOnly then begin
    FWriteAccess := asDisabled;
    FDeleteAccess := asDisabled;
    FAddAccess := asDisabled;
    FReplaceAccess := asDisabled;
  end else begin
    FWriteAccess := asUnknown;
    if FStorageForceAbilities.AllowAdd then begin
      FAddAccess := asUnknown;
    end else begin
      FAddAccess := asDisabled;
    end;
    if FStorageForceAbilities.AllowDelete then begin
      FDeleteAccess := asUnknown;
    end else begin
      FDeleteAccess := asDisabled;
    end;
    if FStorageForceAbilities.AllowReplace then begin
      FReplaceAccess := asUnknown;
    end else begin
      FReplaceAccess := asDisabled;
    end;
    if (FDeleteAccess = asDisabled) and (FAddAccess = asDisabled) and (FReplaceAccess = asDisabled) then begin
      FWriteAccess := asDisabled;
    end;
  end;
  FStatic := CreateStatic;
end;

function TStorageStateInternal.CreateStatic: IStorageStateStatic;
begin
  Result :=
    TStorageStateStatic.Create(
      FReadAccess,
      FWriteAccess,
      FDeleteAccess,
      FAddAccess,
      FReplaceAccess
    );
end;

function TStorageStateInternal.GetAddAccess: TAccesState;
begin
  CS.BeginRead;
  try
    Result := FAddAccess;
  finally
    CS.EndRead;
  end;
end;

function TStorageStateInternal.GetDeleteAccess: TAccesState;
begin
  CS.BeginRead;
  try
    Result := FDeleteAccess;
  finally
    CS.EndRead;
  end;
end;

function TStorageStateInternal.GetReadAccess: TAccesState;
begin
  CS.BeginRead;
  try
    Result := FReadAccess;
  finally
    CS.EndRead;
  end;
end;

function TStorageStateInternal.GetReplaceAccess: TAccesState;
begin
  CS.BeginRead;
  try
    Result := FReplaceAccess;
  finally
    CS.EndRead;
  end;
end;

function TStorageStateInternal.GetWriteAccess: TAccesState;
begin
  CS.BeginRead;
  try
    Result := FWriteAccess;
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

procedure TStorageStateInternal.SetAddAccess(AValue: TAccesState);
var
  VValue: TAccesState;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := AValue;
  if FStorageForceAbilities.IsReadOnly or not FStorageForceAbilities.AllowAdd then begin
    VValue := asDisabled;
  end;

  CS.BeginWrite;
  try
    if FAddAccess <> VValue then begin
      FAddAccess := VValue;
      if (FDeleteAccess = asDisabled) and (FAddAccess = asDisabled) and (FReplaceAccess = asDisabled) then begin
        FWriteAccess := asDisabled;
      end;
      if (FAddAccess = asEnabled) then begin
        if (FWriteAccess <> asEnabled) then begin
          FWriteAccess := asEnabled;
        end;
        if (FReadAccess <> asEnabled) then begin
          FReadAccess := asEnabled;
        end;
      end;
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

procedure TStorageStateInternal.SetDeleteAccess(AValue: TAccesState);
var
  VValue: TAccesState;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := AValue;
  if FStorageForceAbilities.IsReadOnly or not FStorageForceAbilities.AllowDelete then begin
    VValue := asDisabled;
  end;

  CS.BeginWrite;
  try
    if FDeleteAccess <> VValue then begin
      FDeleteAccess := VValue;
      if (FDeleteAccess = asDisabled) and (FAddAccess = asDisabled) and (FReplaceAccess = asDisabled) then begin
        FWriteAccess := asDisabled;
      end;
      if (FDeleteAccess = asEnabled) then begin
        if (FWriteAccess <> asEnabled) then begin
          FWriteAccess := asEnabled;
        end;
        if (FReadAccess <> asEnabled) then begin
          FReadAccess := asEnabled;
        end;
      end;
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

procedure TStorageStateInternal.SetReadAccess(AValue: TAccesState);
var
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    if FReadAccess <> AValue then begin
      FReadAccess := AValue;
      if FAddAccess = asDisabled then begin
        FWriteAccess := asDisabled;
        FDeleteAccess := asDisabled;
        FAddAccess := asDisabled;
        FReplaceAccess := asDisabled;
      end;
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

procedure TStorageStateInternal.SetReplaceAccess(AValue: TAccesState);
var
  VValue: TAccesState;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := AValue;
  if FStorageForceAbilities.IsReadOnly or not FStorageForceAbilities.AllowReplace then begin
    VValue := asDisabled;
  end;

  CS.BeginWrite;
  try
    if FReplaceAccess <> VValue then begin
      FReplaceAccess := VValue;
      if (FDeleteAccess = asDisabled) and (FAddAccess = asDisabled) and (FReplaceAccess = asDisabled) then begin
        FWriteAccess := asDisabled;
      end;
      if (FReplaceAccess = asEnabled) then begin
        if (FWriteAccess <> asEnabled) then begin
          FWriteAccess := asEnabled;
        end;
        if (FReadAccess <> asEnabled) then begin
          FReadAccess := asEnabled;
        end;
      end;
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

procedure TStorageStateInternal.SetWriteAccess(AValue: TAccesState);
var
  VValue: TAccesState;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := AValue;
  if FStorageForceAbilities.IsReadOnly or not (
    FStorageForceAbilities.AllowReplace or
    FStorageForceAbilities.AllowAdd or
    FStorageForceAbilities.AllowDelete
    ) then begin
    VValue := asDisabled;
  end;

  CS.BeginWrite;
  try
    if (FDeleteAccess = asDisabled) and (FAddAccess = asDisabled) and (FReplaceAccess = asDisabled) then begin
      VValue := asDisabled;
    end;
    if FWriteAccess <> VValue then begin
      FWriteAccess := VValue;
      if FWriteAccess = asDisabled then begin
        FDeleteAccess := asDisabled;
        FAddAccess := asDisabled;
        FReplaceAccess := asDisabled;
      end;
      if (FWriteAccess = asEnabled) then begin
        if (FReadAccess <> asEnabled) then begin
          FReadAccess := asEnabled;
        end;
      end;
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
