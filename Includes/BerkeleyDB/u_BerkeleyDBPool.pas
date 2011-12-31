{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  Windows, // for INFINITE constant only
  Classes,
  SysUtils,
  SyncObjs,
  u_BerkeleyDB;

type
  PBDBPoolRec = ^TBDBPoolRec;
  TBDBPoolRec = record
    Obj: TBerkeleyDB;
    AcquireTime: TDateTime;
    ReleaseTime: TDateTime;
    ActiveCount: Integer;
  end;

  TBerkeleyDBPool = class(TObject)
  private
    FCS: TCriticalSection;
    FObjList: TList;
    FPoolSize: Integer;
    FActive: Boolean;
    FUsageCount: Integer;
    FFinishEvent: TEvent;
    function GetPoolSize: Integer;
    procedure SetPoolSize(AValue: Integer);
    procedure Abort;
  public
    constructor Create;
    destructor Destroy; override;
    function Acquire(const AFileName: string): TBerkeleyDB;
    procedure Release(AObj: TBerkeleyDB);
    property Size: Integer read GetPoolSize write SetPoolSize;
  end;

implementation

{ TBerkeleyDBPool }

constructor TBerkeleyDBPool.Create;
begin
  FCS := TCriticalSection.Create;
  FObjList := TList.Create;
  FFinishEvent := TEvent.Create;
  FPoolSize := 12;
  FUsageCount := 0;
  FActive := True;
end;

destructor TBerkeleyDBPool.Destroy;
begin
  Abort;
  FObjList.Free;
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
  FCS.Acquire;
  try
    VFound := False;
    for I := 0 to FObjList.Count - 1 do begin
      PRec := FObjList.Items[I];
      VFound := (PRec.Obj = AObj);
      if VFound then begin
        Dec(FUsageCount);
        PRec.ReleaseTime := Now;
        Dec(PRec.ActiveCount);
        Break;
      end;
    end;
    if not VFound then begin
      raise Exception.Create('Error [BerkeleyDB]: Can''t release an object that is not in the pool!');
    end;
    if not FActive and (FUsageCount <= 0) then begin
      FFinishEvent.SetEvent;
    end;
  finally
    FCS.Release;
  end;
end;

function TBerkeleyDBPool.Acquire(const AFileName: string): TBerkeleyDB;
var
  I: Integer;
  PRec: PBDBPoolRec;
  VFound: Boolean;
  VRecIndexOldest: Integer;
  VReleaseOldest: TDateTime;
begin
  Result := nil;
  FCS.Acquire;
  try
    if FActive then begin
      VRecIndexOldest := -1;
      VReleaseOldest := MaxDateTime;
      VFound := False;
      // Ищем среди открытых
      for I := 0 to FObjList.Count - 1 do begin
        PRec := FObjList.Items[I];
        VFound := (PRec.Obj.FileName = AFileName);
        if VFound then begin
          PRec.AcquireTime := Now;
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
      // Среди открытых не нашли
      if not VFound then begin
        if FObjList.Count < FPoolSize then begin
          // если не достигли пределов пула, создаём новый объект
          New(PRec);
          try
            PRec.Obj := TBerkeleyDB.Create;
          except
            Dispose(PRec);
            raise;
          end;
          PRec.Obj.FileName := AFileName;
          PRec.AcquireTime := Now;
          PRec.ReleaseTime := MinDateTime;
          PRec.ActiveCount := 1;
          FObjList.Add(PRec);
          Result := PRec.Obj;
        end else if VRecIndexOldest <> -1 then begin
          // иначе, пытаемся закрыть старую неиспользующуюся БД
          PRec := FObjList.Items[VRecIndexOldest];
          if (PRec <> nil) and (PRec.ActiveCount <= 0) then begin
            FreeAndNil(PRec.Obj);
            try
              PRec.Obj := TBerkeleyDB.Create;
            except
              Dispose(PRec);
              FObjList.Delete(VRecIndexOldest);
              raise;
            end;
            PRec.Obj.FileName := AFileName;
            PRec.AcquireTime := Now;
            PRec.ReleaseTime := MinDateTime;
            PRec.ActiveCount := 1;
            Result := PRec.Obj;
          end;
        end else begin
          raise Exception.Create('Error [BerkeleyDB]: There are no available objects in the pool!');
        end;
      end;
      if Result <> nil then begin
        Inc(FUsageCount);
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
      raise Exception.Create('Error [BerkeleyDB]: Can''t decrease pool size!');
    end;
  finally
    FCS.Release;
  end;
end;

end.
