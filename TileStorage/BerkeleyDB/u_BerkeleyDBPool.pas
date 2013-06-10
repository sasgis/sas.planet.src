{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_BerkeleyDBPool;

interface

uses
  Windows,
  Classes,
  SysUtils,
  SyncObjs,
  i_BerkeleyDBFactory,
  i_GlobalBerkeleyDBHelper,
  i_BerkeleyDB,
  i_BerkeleyDBPool,
  u_BaseInterfacedObject;

type
  TBerkeleyDBPool = class(TBaseInterfacedObject, IBerkeleyDBPool)
  private
    FHelper: IGlobalBerkeleyDBHelper;
    FDatabaseFactory: IBerkeleyDBFactory;
    FCS: TCriticalSection;
    FObjList: TList;
    FPoolSize: Integer;
    FUnusedObjectTTL: Cardinal;
    FActive: Boolean;
    FUsageCount: Integer;
    FFinishEvent: TEvent;
    FCrashList: TStringList;
    procedure Abort;
  private
    { IBerkeleyDBPool }
    function Acquire(const ADatabaseFileName: string): IBerkeleyDB;
    procedure Release(const ADatabase: IBerkeleyDB);
    function Count: Integer;
    procedure Sync;
  public
    constructor Create(
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const ADatabaseFactory: IBerkeleyDBFactory;
      const APoolSize: Cardinal;
      const AUnusedObjectTTL: Cardinal
    );
    destructor Destroy; override;
  end;

implementation

type
  TPoolRec = record
    Database: IBerkeleyDB;
    AcquireTime: Cardinal;
    ReleaseTime: Cardinal;
    ActiveCount: Integer;
  end;
  PPoolRec = ^TPoolRec;

type
  EBerkeleyDBPool = class(Exception);

resourcestring
  rsObjectNotInPool = 'Can''t release an object that is not in the pool!';
  rsNoAvailableObjects = 'There are no available objects in the pool!';
  rsCantAcquireDB = 'Can''t acquire db: %s';
  rsPoolIsDesabled = 'Pool Disabled - Can''t acquire db: %s';
  rsCantUseOldPoolRecord = 'Can''t use old pool record!';

{ TBerkeleyDBPool }

constructor TBerkeleyDBPool.Create(
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const ADatabaseFactory: IBerkeleyDBFactory;
  const APoolSize: Cardinal;
  const AUnusedObjectTTL: Cardinal
);
begin
  inherited Create;
  FHelper := AGlobalBerkeleyDBHelper;
  FDatabaseFactory := ADatabaseFactory;
  FCS := TCriticalSection.Create;
  FObjList := TList.Create;
  FCrashList := TStringList.Create;
  FCrashList.CaseSensitive := False;
  FFinishEvent := TEvent.Create;
  FPoolSize := APoolSize;
  FUnusedObjectTTL := AUnusedObjectTTL;
  FUsageCount := 0;
  FActive := True;
end;

destructor TBerkeleyDBPool.Destroy;
begin
  Abort;
  FreeAndNil(FObjList);
  FreeAndNil(FCrashList);
  FreeAndNil(FCS);
  FreeAndNil(FFinishEvent);
  FDatabaseFactory := nil;
  FHelper := nil;
  inherited;
end;

procedure TBerkeleyDBPool.Release(const ADatabase: IBerkeleyDB);
var
  I: Integer;
  PRec: PPoolRec;
  VFound: Boolean;
begin
  try
    if Assigned(ADatabase) then begin
      FCS.Acquire;
      try
        VFound := False;
        for I := 0 to FObjList.Count - 1 do begin
          PRec := FObjList.Items[I];
          if PRec <> nil then begin
            VFound := ((PRec.Database as IInterface) = (ADatabase as IInterface));
            if VFound then begin
              Dec(FUsageCount);
              PRec.ReleaseTime := GetTickCount;
              Dec(PRec.ActiveCount);
              Break;
            end;
          end;
        end;
        if not VFound then begin
          raise EBerkeleyDBPool.Create(rsObjectNotInPool);
        end;
        if not FActive and (FUsageCount <= 0) then begin
          FFinishEvent.SetEvent;
        end;
      finally
        FCS.Release;
      end;
    end;
  except
    on E: Exception do
      FHelper.RaiseException(E.ClassName + ': ' + E.Message);
  end;
end;

function TBerkeleyDBPool.Acquire(const ADatabaseFileName: string): IBerkeleyDB;
var
  I: Integer;
  PRec: PPoolRec;
  VFound: Boolean;
  VRecIndex: Integer;
  VRecIndexOldest: Integer;
  VReleaseOldest: Cardinal;
begin
  try
    Result := nil;
    FCS.Acquire;
    try
      if FActive then begin
        if not FCrashList.Find(ADatabaseFileName, I) then begin
          VRecIndexOldest := -1;
          VReleaseOldest := $FFFFFFFF;
          VFound := False;
          // Ищем среди открытых
          for I := 0 to FObjList.Count - 1 do begin
            PRec := FObjList.Items[I];
            if PRec <> nil then begin
              VFound := (PRec.Database.FileName = ADatabaseFileName);
              if VFound then begin
                PRec.AcquireTime := GetTickCount;
                Inc(PRec.ActiveCount);
                Result := PRec.Database;
                Break;
              end else if PRec.ActiveCount <= 0 then begin
                // попутно находим наистарейшую неиспользующуюся БД
                if PRec.ReleaseTime < VReleaseOldest then begin
                  VReleaseOldest := PRec.ReleaseTime;
                  VRecIndexOldest := I;
                end;
              end;
            end;
          end;
          // Среди открытых не нашли
          if not VFound then begin
             if FObjList.Count < FPoolSize then begin
              New(PRec); // если не достигли пределов пула, создаём новую запись
              VRecIndex := FObjList.Add(PRec);
            end else if VRecIndexOldest <> -1 then begin
              // иначе, используем старую
              PRec := FObjList.Items[VRecIndexOldest];
              if (PRec <> nil) and (PRec.ActiveCount <= 0) then begin
                VRecIndex := VRecIndexOldest;
                PRec.Database := nil;
              end else begin
                raise EBerkeleyDBPool.Create(rsCantUseOldPoolRecord);
              end;
            end else begin
              // fail - пул заполнен и нету неиспользующихся старых записей
              raise EBerkeleyDBPool.Create(rsNoAvailableObjects);
            end;
            PRec := FObjList.Items[VRecIndex];
            PRec.Database := FDatabaseFactory.CreateDatabase(ADatabaseFileName);
            PRec.AcquireTime := GetTickCount;
            PRec.ReleaseTime := 0;
            PRec.ActiveCount := 1;               
            Result := PRec.Database;
          end;

          if Result <> nil then begin
            Inc(FUsageCount);
          end else begin
            raise EBerkeleyDBPool.CreateFmt(rsCantAcquireDB, [ADatabaseFileName]);
          end;
        end;
      end else begin
        raise EBerkeleyDBPool.CreateFmt(rsPoolIsDesabled, [ADatabaseFileName]);
      end;
    finally
      FCS.Release;
    end;
  except
    on E: Exception do begin
      FCrashList.Add(ADatabaseFileName);
      FHelper.RaiseException(E.ClassName + ': ' + E.Message);
    end;
  end;
end;

procedure TBerkeleyDBPool.Sync;
var
  I: Integer;
  PRec: PPoolRec;
begin
  FCS.Acquire;
  try
    try
      for I := 0 to FObjList.Count - 1 do begin
        PRec := FObjList.Items[i];
        if (PRec <> nil) then begin
          if (PRec.ActiveCount <= 0) and ((GetTickCount - PRec.ReleaseTime) > FUnusedObjectTTL) then begin
            PRec.Database := nil;
            Dispose(PRec);
            FObjList.Items[i] := nil;
          end else begin
            PRec.Database.Sync;
          end;
        end;
      end;
    finally
      FObjList.Pack;
    end;
  finally
    FCS.Release;
  end;
end;

function TBerkeleyDBPool.Count: Integer;
begin
  FCS.Acquire;
  try
    if Assigned(FObjList) then begin
      Result := FObjList.Count;
    end else begin
      Result := 0;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TBerkeleyDBPool.Abort;
var
  I: Integer;
  PRec: PPoolRec;
begin
  FCS.Acquire;
  try
    FActive := False;
    if FUsageCount <= 0 then begin
      FFinishEvent.SetEvent;
    end;
  finally
   FCS.Release;
  end;

  FFinishEvent.WaitFor(INFINITE);

  FCS.Acquire;
  try
    for I := 0 to FObjList.Count - 1 do begin
      PRec := FObjList.Items[i];
      if PRec <> nil then begin
        PRec.Database := nil;
        Dispose(PRec);
      end;
    end;
    FObjList.Clear;
  finally
    FCS.Release;
  end;
end;

end.
