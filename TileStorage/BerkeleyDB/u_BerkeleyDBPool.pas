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
  u_BerkeleyDB;

type
  PBDBPoolRec = ^TBDBPoolRec;
  TBDBPoolRec = record
    Obj: TBerkeleyDB;
    AcquireTime: Cardinal;
    ReleaseTime: Cardinal;
    ActiveCount: Integer;
  end;

  TOnObjCreate = function(const AFileName: string): TBerkeleyDB of object;

  TBerkeleyDBPool = class(TObject)
  private
    FCS: TCriticalSection;
    FObjList: TList;
    FPoolSize: Integer;
    FActive: Boolean;
    FUsageCount: Integer;
    FFinishEvent: TEvent;
    FOnObjCreate: TOnObjCreate;
    FCrashList: TStringList;
    function GetPoolSize: Integer;
    procedure SetPoolSize(AValue: Integer);
    procedure Abort;
  public
    constructor Create(const APoolSize: Cardinal = 12);
    destructor Destroy; override;
    procedure Sync;
    function Acquire(const AFileName: string): TBerkeleyDB;
    procedure Release(AObj: TBerkeleyDB);
    property Size: Integer read GetPoolSize write SetPoolSize;
    property OnObjCreate: TOnObjCreate read FOnObjCreate write FOnObjCreate;
  end;

implementation

uses
  libdb51,
  u_BerkeleyDBErrorHandler;

{ TBerkeleyDBPool }

constructor TBerkeleyDBPool.Create(const APoolSize: Cardinal = 12);
begin
  FCS := TCriticalSection.Create;
  FObjList := TList.Create;
  FCrashList := TStringList.Create;
  FCrashList.CaseSensitive := False;
  FFinishEvent := TEvent.Create;
  FPoolSize := APoolSize;
  FUsageCount := 0;
  FOnObjCreate := nil;
  FActive := True;
end;

destructor TBerkeleyDBPool.Destroy;
begin
  Abort;
  FObjList.Free;
  FCrashList.Free;
  FCS.Free;
  FreeAndNil(FFinishEvent);
  inherited;
end;

procedure TBerkeleyDBPool.Release(AObj: TBerkeleyDB);
var
  I: Integer;
  PRec: PBDBPoolRec;
  VFound: Boolean;
begin
  if Assigned(AObj) then begin
    FCS.Acquire;
    try
      VFound := False;
      for I := 0 to FObjList.Count - 1 do begin
        PRec := FObjList.Items[I];
        if PRec <> nil then begin
          VFound := (PRec.Obj = AObj);
          if VFound then begin
            Dec(FUsageCount);
            PRec.ReleaseTime := GetTickCount;
            Dec(PRec.ActiveCount);
            Break;
          end;
        end;
      end;
      if not VFound then begin
        BDBRaiseException(
          'Error [BerkeleyDB]: Can''t release an object that is not in the pool!'
        );
      end;
      if not FActive and (FUsageCount <= 0) then begin
        FFinishEvent.SetEvent;
      end;
    finally
      FCS.Release;
    end;
  end;
end;

function TBerkeleyDBPool.Acquire(const AFileName: string): TBerkeleyDB;

  function CreateNewObj(): TBerkeleyDB;
  var
    PRec: PBDBPoolRec;
    VObj: TBerkeleyDB;
  begin
    Result := nil;
    if Addr(FOnObjCreate) <> nil then begin
      VObj := FOnObjCreate(AFileName);
    end else begin
      VObj := TBerkeleyDB.Create;
    end;
    if Assigned(VObj) then begin
      New(PRec);
      PRec.Obj := VObj;
      PRec.AcquireTime := GetTickCount;
      PRec.ReleaseTime := 0;
      PRec.ActiveCount := 1;
      FObjList.Add(PRec);
      Result := VObj;
    end;
  end;

var
  I: Integer;
  PRec: PBDBPoolRec;
  VFound: Boolean;
  VRecIndexOldest: Integer;
  VReleaseOldest: Cardinal;
begin
  Result := nil;
  FCS.Acquire;
  try
    if FActive then begin
      if not FCrashList.Find(AFileName, I) then
      try
        VRecIndexOldest := -1;
        VReleaseOldest := $FFFFFFFF;
        VFound := False;
        // Ищем среди открытых
        for I := 0 to FObjList.Count - 1 do begin
          PRec := FObjList.Items[I];
          if PRec <> nil then begin
            VFound := (PRec.Obj.FileName = AFileName);
            if VFound then begin
              PRec.AcquireTime := GetTickCount;
              Inc(PRec.ActiveCount);
              Result := PRec.Obj;
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
            // если не достигли пределов пула, создаём новый объект
            Result := CreateNewObj();
          end else if VRecIndexOldest <> -1 then begin
            // иначе, пытаемся закрыть старую неиспользующуюся БД
            PRec := FObjList.Items[VRecIndexOldest];
            if (PRec <> nil) and (PRec.ActiveCount <= 0) then begin
              FreeAndNil(PRec.Obj);
              Dispose(PRec);
              FObjList.Delete(VRecIndexOldest);
              FObjList.Pack;
              Result := CreateNewObj();
            end;
          end else begin
            BDBRaiseException(
              'Error [BerkeleyDB]: There are no available objects in the pool!'
            );
          end;
        end;
        if Result <> nil then begin
          Inc(FUsageCount);
        end else begin
          BDBRaiseException(
            'Error [BerkeleyDB]: Can''t acquire db: ' + AnsiString(AFileName)
          );
        end;
      except
        FCrashList.Add(AFileName);
        raise;
      end;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TBerkeleyDBPool.Abort;
var
  I: Integer;
  PRec: PBDBPoolRec;
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
        if Assigned(PRec.Obj) then begin
          FreeAndNil(PRec.Obj);
        end;
        Dispose(PRec);
      end;
    end;
    FObjList.Clear;
  finally
    FCS.Release;
  end;
end;

procedure TBerkeleyDBPool.Sync;
const
  CMaxReleaseTimeToCloseByTTL = 30000; // 30 sec
var
  I: Integer;
  PRec: PBDBPoolRec;
begin
  FCS.Acquire;
  try
    try
      for I := 0 to FObjList.Count - 1 do begin
        PRec := FObjList.Items[i];
        if (PRec <> nil) and Assigned(PRec.Obj) then begin
          if (PRec.ActiveCount <= 0) then begin
            if GetTickCount - PRec.ReleaseTime > CMaxReleaseTimeToCloseByTTL then begin
              FreeAndNil(PRec.Obj);
              Dispose(PRec);
              FObjList.Items[i] := nil;
            end else begin
              PRec.Obj.Sync;
            end;
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

function TBerkeleyDBPool.GetPoolSize: Integer;
begin
  FCS.Acquire;
  try
    Result := FPoolSize;
  finally
    FCS.Release;
  end;
end;

procedure TBerkeleyDBPool.SetPoolSize(AValue: Integer);
begin
  FCS.Acquire;
  try
    if AValue > FPoolSize then begin
      FPoolSize := AValue;
    end else begin
      BDBRaiseException(
        'Error [BerkeleyDB]: Can''t decrease pool size!'
      );
    end;
  finally
    FCS.Release;
  end;
end;

end.
