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

unit u_MarkSystemORMTransaction;

interface

uses
  Windows,
  mormot.orm.core,
  mormot.orm.rest,
  i_MarkSystemImplORMClientProvider,
  u_BaseInterfacedObject;

type
  TMarkSystemORMTransaction = class(TBaseInterfacedObject, IMarkSystemORMTransaction)
  private
    FClient: TRestOrm;
  private
    { IMarkSystemORMTransaction }
    function Start(const AOrmClass: TOrmClass; const AIsReadOnly: Boolean): TTransactionRec;
    procedure Commit(var ATrans: TTransactionRec);
    procedure RollBack(var ATrans: TTransactionRec);
  public
    constructor Create(const AClient: TRestOrm);
  end;

  TMarkSystemORMTransactionNoOp = class(TBaseInterfacedObject, IMarkSystemORMTransaction)
  private
    { IMarkSystemORMTransaction }
    function Start(const AOrmClass: TOrmClass; const AIsReadOnly: Boolean): TTransactionRec;
    procedure Commit(var ATrans: TTransactionRec);
    procedure RollBack(var ATrans: TTransactionRec);
  end;

implementation

uses
  t_MarkSystemORM;

{ TMarkSystemORMTransaction }

constructor TMarkSystemORMTransaction.Create(const AClient: TRestOrm);
begin
  inherited Create;
  FClient := AClient;
end;

function TMarkSystemORMTransaction.Start(const AOrmClass: TOrmClass; const AIsReadOnly: Boolean): TTransactionRec;
begin
  Result.FIsReadOnly := AIsReadOnly;
  if Result.FIsReadOnly then begin
    Exit;
  end;
  Result.FSessionID := FClient.TransactionActiveSession;
  if Result.FSessionID = 0 then begin
    Result.FSessionID := GetTickCount;
    if not FClient.TransactionBegin(AOrmClass, Result.FSessionID) then begin
      raise EMarkSystemORMError.Create('MarkSystemORM: Start transaction is failed!');
    end;
    Result.FIsInternal := True;
  end else begin
    Result.FIsInternal := False;
  end;
end;

procedure TMarkSystemORMTransaction.Commit(var ATrans: TTransactionRec);
begin
  if ATrans.FIsReadOnly then begin
    Exit;
  end;
  Assert(ATrans.FSessionID > 0);
  if ATrans.FIsInternal and (ATrans.FSessionID > 0) then begin
    FClient.Commit(ATrans.FSessionID, True);
    ATrans.FSessionID := 0;
  end;
end;

procedure TMarkSystemORMTransaction.RollBack(var ATrans: TTransactionRec);
begin
  if ATrans.FIsReadOnly then begin
    Exit;
  end;
  Assert(ATrans.FSessionID > 0);
  if ATrans.FIsInternal and (ATrans.FSessionID > 0) then begin
    FClient.RollBack(ATrans.FSessionID);
    ATrans.FSessionID := 0;
  end;
end;

{ TMarkSystemORMTransactionNoOp }

function TMarkSystemORMTransactionNoOp.Start(const AOrmClass: TOrmClass; const AIsReadOnly: Boolean): TTransactionRec;
begin
  Result.FIsReadOnly := True;
end;

procedure TMarkSystemORMTransactionNoOp.Commit(var ATrans: TTransactionRec);
begin
  //
end;

procedure TMarkSystemORMTransactionNoOp.RollBack(var ATrans: TTransactionRec);
begin
  //
end;

end.
