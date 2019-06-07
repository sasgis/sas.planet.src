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

unit u_PascalScriptGlobal;

interface

uses
  SysUtils,
  Variants,
  SynCommons,
  i_PascalScriptGlobal,
  u_BaseInterfacedObject;

type
  TPascalScriptGlobal = class(TBaseInterfacedObject, IPascalScriptGlobal)
  private
    type
      TItemRec = packed record
        ID: Integer;
        Value: Variant;
      end;
      TItemsRecArray = array of TItemRec;
  private
    FLock: IReadWriteSync;
    FItems: TItemsRecArray;
    FItemsA: TDynArray;
    FItemsACount: Integer;
  private
    { IPascalScriptGlobal }
    procedure Lock;
    procedure Unlock;

    procedure LockRead;
    procedure UnlockRead;

    procedure SetVar(const AVarID: Integer; const AValue: Variant); inline;
    procedure SetVarTS(const AVarID: Integer; const AValue: Variant);

    function GetVar(const AVarID: Integer): Variant; inline;
    function GetVarTS(const AVarID: Integer): Variant;

    function Exists(const AVarID: Integer): Boolean; inline;
    function ExistsTS(const AVarID: Integer): Boolean;
  public
    constructor Create;
  end;

implementation

uses
  u_Synchronizer;

{ TPascalScriptGlobal }

constructor TPascalScriptGlobal.Create;
begin
  inherited Create;
  FLock := GSync.SyncStd.Make(Self.ClassName);
  FItemsACount := 0;
  FItemsA.InitSpecific(TypeInfo(TItemsRecArray), FItems, djInteger, @FItemsACount);
  FItemsA.Sorted := True;
end;

procedure TPascalScriptGlobal.Lock;
begin
  FLock.BeginWrite;
end;

procedure TPascalScriptGlobal.Unlock;
begin
  FLock.EndWrite;
end;

procedure TPascalScriptGlobal.LockRead;
begin
  FLock.BeginRead;
end;

procedure TPascalScriptGlobal.UnlockRead;
begin
  FLock.EndRead;
end;

function TPascalScriptGlobal.Exists(const AVarID: Integer): Boolean;
var
  I: Integer;
begin
  Result := FItemsA.FastLocateSorted(AVarID, I);
end;

function TPascalScriptGlobal.ExistsTS(const AVarID: Integer): Boolean;
begin
  FLock.BeginRead;
  try
    Result := Exists(AVarID);
  finally
    FLock.EndRead;
  end;
end;

procedure TPascalScriptGlobal.SetVar(const AVarID: Integer; const AValue: Variant);
var
  I: Integer;
  VItem: TItemRec;
  VWasAdded: Boolean;
begin
  VItem.ID := AVarID;
  VItem.Value := AValue;
  I := FItemsA.FastLocateOrAddSorted(VItem, @VWasAdded);
  if not VWasAdded then begin
    FItems[I].Value := AValue;
  end;
end;

procedure TPascalScriptGlobal.SetVarTS(const AVarID: Integer; const AValue: Variant);
begin
  FLock.BeginWrite;
  try
    SetVar(AVarID, AValue);
  finally
    FLock.EndWrite;
  end;
end;

function TPascalScriptGlobal.GetVar(const AVarID: Integer): Variant;
var
  I: Integer;
begin
  if FItemsA.FastLocateSorted(AVarID, I) then begin
    Result := FItems[I].Value;
  end else begin
    Result := Unassigned;
  end;
end;

function TPascalScriptGlobal.GetVarTS(const AVarID: Integer): Variant;
begin
  FLock.BeginRead;
  try
    Result := GetVar(AVarID);
  finally
    FLock.EndRead;
  end;
end;

end.
