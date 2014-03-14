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

unit u_ReadWriteStateInternal;

interface

uses
  t_CommonTypes,
  i_ReadWriteState,
  i_ReadWriteStateInternal,
  u_ConfigDataElementBase;

type
  TReadWriteStateInternal = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IReadWriteStateInternal, IReadWriteStateChangeble)
  private
    FReadAccess: TAccesState;
    FWriteAccess: TAccesState;
  protected
    function CreateStatic: IInterface; override;
  private
    function GetReadAccess: TAccesState;
    procedure SetReadAccess(AValue: TAccesState);

    function GetWriteAccess: TAccesState;
    procedure SetWriteAccess(AValue: TAccesState);

    function GetStatic: IReadWriteStateStatic;
  public
    constructor Create;
  end;

implementation

uses
  u_ReadWriteStateStatic;

{ TReadWriteStateInternal }

constructor TReadWriteStateInternal.Create;
begin
  inherited Create;
  FReadAccess := asUnknown;
  FWriteAccess := asUnknown;
end;

function TReadWriteStateInternal.CreateStatic: IInterface;
var
  VStatic: IReadWriteStateStatic;
begin
  VStatic :=
    TReadWriteStateStatic.Create(
      FReadAccess,
      FWriteAccess
    );
  Result := VStatic;
end;

function TReadWriteStateInternal.GetReadAccess: TAccesState;
begin
  LockRead;
  try
    Result := FReadAccess;
  finally
    UnlockRead;
  end;
end;

function TReadWriteStateInternal.GetWriteAccess: TAccesState;
begin
  LockRead;
  try
    Result := FWriteAccess;
  finally
    UnlockRead;
  end;
end;

function TReadWriteStateInternal.GetStatic: IReadWriteStateStatic;
begin
  Result := IReadWriteStateStatic(GetStaticInternal);
end;

procedure TReadWriteStateInternal.SetReadAccess(AValue: TAccesState);
begin
  LockWrite;
  try
    if FReadAccess <> AValue then begin
      FReadAccess := AValue;
      if FReadAccess = asDisabled then begin
        FWriteAccess := asDisabled;
      end;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TReadWriteStateInternal.SetWriteAccess(AValue: TAccesState);
begin
  LockWrite;
  try
    if FWriteAccess <> AValue then begin
      FWriteAccess := AValue;
      if (FWriteAccess = asEnabled) then begin
        if (FReadAccess <> asEnabled) then begin
          FReadAccess := asEnabled;
        end;
      end;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
