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

unit u_PoolOfObjectsSimple;

interface

uses
  Windows,
  Classes,
  SyncObjs,
  i_JclNotify,
  i_OperationNotifier,
  i_SimpleFactory,
  i_ObjectWithTTL,
  i_PoolElement,
  i_IPoolOfObjectsSimple;

type
  TPoolOfObjectsSimple = class(TInterfacedObject, IPoolOfObjectsSimple, IObjectWithTTL)
  private
    FList: TList;
    FObjectFactory: ISimpleFactory;
    FObjectTimeToLive: Cardinal;
    FCheckInterval: Cardinal;
    FOldestObjectTime: Cardinal;
    FLastCheckTime: Cardinal;
    FSemaphore: THandle;

    FCancelEvent: TEvent;
    FCancelListener: IJclListener;
    procedure OnDownloadCanceled(Sender: TObject);
  protected { IPoolOfObjectsSimple }
    function TryGetPoolElement(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier
    ): IPoolElement;
    function GetPoolSize: Cardinal;
  protected { IObjectWithTTL }
    function GetNextCheckTime: Cardinal;
    procedure TrimByTTL;
  public
    constructor Create(
      APoolSize: Cardinal;
      AObjectFactory: ISimpleFactory;
      AObjectTimeToLive: Cardinal;
      ACheckInterval: Cardinal
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_NotifyEventListener,
  u_PoolElement;

{ TPoolOfObjectsSimple }

constructor TPoolOfObjectsSimple.Create(APoolSize: Cardinal;
  AObjectFactory: ISimpleFactory; AObjectTimeToLive: Cardinal;
  ACheckInterval: Cardinal);
var
  i: integer;
begin
  FCancelEvent := TEvent.Create;
  FCancelListener := TNotifyEventListener.Create(Self.OnDownloadCanceled);

  FObjectFactory := AObjectFactory;
  FObjectTimeToLive := AObjectTimeToLive;
  FCheckInterval := ACheckInterval;
  FList := TList.Create;
  FList.Count := APoolSize;
  FSemaphore := CreateSemaphore(nil, FList.Count, FList.Count, '');
  for i := 0 to FList.Count - 1 do begin
    FList.Items[i] := TPoolElement.Create(FObjectFactory, FSemaphore);
  end;
  FOldestObjectTime := 0;
  FLastCheckTime := GetTickCount;
end;

destructor TPoolOfObjectsSimple.Destroy;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do begin
    TPoolElement(FList.Items[i]).Free;
  end;
  FreeAndNil(FList);
  if FSemaphore <> 0 then begin
    CloseHandle(FSemaphore);
    FSemaphore := 0;
  end;
  FCancelEvent.SetEvent;
  FreeAndNil(FCancelEvent);
  inherited;
end;

function TPoolOfObjectsSimple.GetNextCheckTime: Cardinal;
begin
  if FOldestObjectTime <= 0 then begin
    Result := FLastCheckTime + FCheckInterval;
  end else begin
    Result := FOldestObjectTime + FObjectTimeToLive;
  end;
end;

function TPoolOfObjectsSimple.GetPoolSize: Cardinal;
begin
  Result := FList.Count;
end;

procedure TPoolOfObjectsSimple.OnDownloadCanceled(Sender: TObject);
begin
  FCancelEvent.SetEvent;
end;

procedure TPoolOfObjectsSimple.TrimByTTL;
var
  i: integer;
  VMinTime: Cardinal;
  VLastUse: Cardinal;
  VOldestUse: Cardinal;
  VElement: TPoolElement;
begin
  FLastCheckTime := GetTickCount;
  VMinTime := FLastCheckTime - FObjectTimeToLive;
  VOldestUse := 0;
  for i := 0 to FList.Count - 1 do begin
    VElement := TPoolElement(FList.Items[i]);
    VElement.FreeObjectByTTL(VMinTime);
    VLastUse := VElement.GetLastUseTime;
    if (VLastUse > 0) then begin
      if (VOldestUse <= 0) or ((VOldestUse > 0) and (VLastUse < VOldestUse)) then begin
        VOldestUse := VLastUse;
      end;
    end;
  end;
  FOldestObjectTime := VOldestUse;
end;

function TPoolOfObjectsSimple.TryGetPoolElement(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier
): IPoolElement;
var
  i: integer;
  VHandles: array [0..1] of THandle;
  VWaitResult: DWORD;
begin
  Result := nil;
  if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    ACancelNotifier.AddListener(FCancelListener);
    try
      VHandles[0] := FSemaphore;
      VHandles[1] := FCancelEvent.Handle;
      while Result = nil do begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Break;
        end;
        VWaitResult := WaitForMultipleObjects(Length(VHandles), @VHandles[0], False, INFINITE);
        if VWaitResult = WAIT_OBJECT_0 then begin
          while Result = nil do begin
            if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
              Break;
            end;
            for i := 0 to FList.Count - 1 do begin
              Result := TPoolElement(FList.Items[i]).TryLock;
              if Result <> nil then begin
                Break;
              end;
            end;
          end;
        end;
      end;
    finally
      ACancelNotifier.RemoveListener(FCancelListener);
    end;
  end;
end;

end.
