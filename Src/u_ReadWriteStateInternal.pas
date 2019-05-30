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
  i_ReadWriteState,
  i_ReadWriteStateInternal,
  u_ChangeableBase;

type
  TReadWriteStateInternal = class(TChangeableWithSimpleLockBase, IReadWriteStateInternal, IReadWriteStateChangeble)
  private
    FReadAccess: Boolean;
    FWriteAccess: Boolean;
    FStatic: IReadWriteStateStatic;
    function CreateStatic: IReadWriteStateStatic;
  private
    function GetReadAccess: Boolean;
    procedure SetReadAccess(AValue: Boolean);

    function GetWriteAccess: Boolean;
    procedure SetWriteAccess(AValue: Boolean);

    function GetStatic: IReadWriteStateStatic;
  public
    constructor Create(
      const AReadAccess: Boolean;
      const AWriteAccess: Boolean
    );
  end;

implementation

uses
  u_ReadWriteStateStatic;

{ TReadWriteStateInternal }

constructor TReadWriteStateInternal.Create(
  const AReadAccess: Boolean;
  const AWriteAccess: Boolean
);
begin
  inherited Create;
  FReadAccess := AReadAccess;
  FWriteAccess := AWriteAccess;
  FStatic := CreateStatic;
end;

function TReadWriteStateInternal.CreateStatic: IReadWriteStateStatic;
begin
  Result :=
    TReadWriteStateStatic.Create(
      FReadAccess,
      FWriteAccess
    );
end;

function TReadWriteStateInternal.GetReadAccess: Boolean;
begin
  CS.BeginRead;
  try
    Result := FReadAccess;
  finally
    CS.EndRead;
  end;
end;

function TReadWriteStateInternal.GetWriteAccess: Boolean;
begin
  CS.BeginRead;
  try
    Result := FWriteAccess;
  finally
    CS.EndRead;
  end;
end;

function TReadWriteStateInternal.GetStatic: IReadWriteStateStatic;
begin
  CS.BeginRead;
  try
    Result := FStatic;
  finally
    CS.EndRead;
  end;
end;

procedure TReadWriteStateInternal.SetReadAccess(AValue: Boolean);
var
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    if FReadAccess <> AValue then begin
      FReadAccess := AValue;
      if not FReadAccess then begin
        FWriteAccess := False;
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

procedure TReadWriteStateInternal.SetWriteAccess(AValue: Boolean);
var
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    if FWriteAccess <> AValue then begin
      FWriteAccess := AValue;
      if FWriteAccess and not FReadAccess then begin
        FReadAccess := True;
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
