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

unit u_BerkeleyDBFactory;

interface

uses
  i_BinaryData,
  i_BerkeleyDB,
  i_BerkeleyDBEnv,
  i_BerkeleyDBFactory,
  u_BaseInterfacedObject;

type
  TBerkeleyDBFactory = class(TBaseInterfacedObject, IBerkeleyDBFactory)
  private
    FPageSize: Cardinal;
    FOnDeadLockRetryCount: Integer;
    FIsReadOnly: Boolean;
    FMetaKey: IBinaryData;
    FMetaValue: IBinaryData;
  private
    { IBerkeleyDBFactory }
    function CreateDatabase(
      const ADatabaseFileName: string;
      const AEnvironment: IBerkeleyDBEnvironment
    ): IBerkeleyDB;
  public
    constructor Create(
      const APageSize: Cardinal;
      const AOnDeadLockRetryCount: Integer;
      const AIsReadOnly: Boolean;
      const AMetaKey: IBinaryData;
      const AMetaValue: IBinaryData
    );
  end;

implementation

uses
  SysUtils,
  libdb51,
  t_BerkeleyDB,
  u_BerkeleyDB;

{ TBerkeleyDBFactory }

constructor TBerkeleyDBFactory.Create(
  const APageSize: Cardinal;
  const AOnDeadLockRetryCount: Integer;
  const AIsReadOnly: Boolean;
  const AMetaKey: IBinaryData;
  const AMetaValue: IBinaryData
);
begin
  inherited Create;
  FPageSize := APageSize;
  FOnDeadLockRetryCount := AOnDeadLockRetryCount;
  FIsReadOnly := AIsReadOnly;
  FMetaKey := AMetaKey;
  FMetaValue := AMetaValue;
end;

function TBerkeleyDBFactory.CreateDatabase(
  const ADatabaseFileName: string;
  const AEnvironment: IBerkeleyDBEnvironment
): IBerkeleyDB;
var
  I: Integer;
  VDatabase: IBerkeleyDB;
  VIsNew: Boolean;
  VIsDeadLock: Boolean;
  VTransaction: PBerkeleyTxn;
begin
  Result := nil;

  VDatabase := TBerkeleyDB.Create(
    AEnvironment,
    FIsReadOnly,
    FOnDeadLockRetryCount,
    FPageSize
  );

  VDatabase.LockWrite;
  try
    VIsNew := not FileExists(ADatabaseFileName);

    VDatabase.Open(ADatabaseFileName);

    if VIsNew and not FIsReadOnly then begin
      I := 0;
      VIsDeadLock := False;
      repeat
        Inc(I);
        AEnvironment.TransactionBegin(VTransaction);
        try
          if not VDatabase.Exists(FMetaKey, VTransaction, VIsDeadLock, DB_RMW) then begin
            if VIsDeadLock then begin
              AEnvironment.TransactionAbort(VTransaction);
              Sleep(50);
              Continue;
            end else begin
              if VDatabase.Write(FMetaKey, FMetaValue, VTransaction, VIsDeadLock) then begin
                AEnvironment.TransactionCommit(VTransaction);
                Break;
              end else if VIsDeadLock then begin
                AEnvironment.TransactionAbort(VTransaction);
                Sleep(50);
                Continue;
              end else begin
                AEnvironment.TransactionAbort(VTransaction);
                Assert(False, 'Can''t write MetaKey to ' + ADatabaseFileName);
                Break;
              end;
            end;
          end else begin
            AEnvironment.TransactionAbort(VTransaction);
            Break;
          end;
        except
          on E: Exception do begin
            AEnvironment.TransactionAbort(VTransaction);
            raise E;
          end;
        end;
      until I > FOnDeadLockRetryCount;
      if VIsDeadLock then begin
        CheckBDB(DB_LOCK_DEADLOCK);
      end;
    end;
  finally
    VDatabase.UnLockWrite;
  end;

  Result := VDatabase;
end;

end.
