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

unit u_NotifierOperation;

interface

uses
  Windows,
  SysUtils,
  i_Notifier,
  i_Listener,
  i_NotifierOperation;

type
  TNotifierOperation = class(TInterfacedObject, INotifierOperation, INotifierOperationInternal)
  private
    FNotifier: INotifierInternal;
    FCurrentOperationID: Integer;
  private
    procedure NextOperation;
  private
    function GetCurrentOperation: Integer; stdcall;
    function IsOperationCanceled(AID: Integer): Boolean; stdcall;

    procedure AddListener(AListener: IListener); stdcall;
    procedure RemoveListener(AListener: IListener); stdcall;
  public
    constructor Create;
  end;

  TNotifierOneOperation = class(TInterfacedObject, INotifierOneOperation, INotifierOneOperationInternal)
  private
    FExecutedCount: Integer;
    FNotifier: INotifierInternal;
    FCS: IReadWriteSync;
  private
    procedure ExecuteOperation;
  private
    function GetIsExecuted: Boolean;

    procedure AddListener(AListener: IListener); stdcall;
    procedure RemoveListener(AListener: IListener); stdcall;
  public
    constructor Create;
  end;

implementation

uses
  u_Synchronizer,
  u_Notifier;

{ TNotifierOperation }

constructor TNotifierOperation.Create;
begin
  inherited Create;
  FNotifier := TNotifierBase.Create;
  FCurrentOperationID := 0;
end;

procedure TNotifierOperation.AddListener(AListener: IListener);
begin
  FNotifier.Add(AListener);
end;

function TNotifierOperation.IsOperationCanceled(AID: Integer): Boolean;
begin
  Result := InterlockedCompareExchange(FCurrentOperationID, 0, 0) <> AID;
end;

function TNotifierOperation.GetCurrentOperation: Integer;
begin
  Result := InterlockedCompareExchange(FCurrentOperationID, 0, 0);
end;

procedure TNotifierOperation.NextOperation;
begin
  InterlockedIncrement(FCurrentOperationID);
  FNotifier.Notify(nil);
end;

procedure TNotifierOperation.RemoveListener(AListener: IListener);
begin
  FNotifier.Remove(AListener);
end;

{ TNotifierOneOperation }

constructor TNotifierOneOperation.Create;
begin
  FCS := MakeSyncRW_Std(Self, TRUE);
  FNotifier := TNotifierBase.Create;
  FExecutedCount := 0;
end;

procedure TNotifierOneOperation.AddListener(AListener: IListener);
var
  VNotifier: INotifierInternal;
begin
  if not GetIsExecuted then begin
    FCS.BeginRead;
    try
      VNotifier := FNotifier;
    finally
      FCS.EndRead;
    end;
    if VNotifier <> nil then begin
      // sync internally
      VNotifier.Add(AListener);
    end;
  end;
end;

procedure TNotifierOneOperation.ExecuteOperation;
var
  VNotifier: INotifierInternal;
begin
  if InterlockedIncrement(FExecutedCount) = 1 then begin
    FCS.BeginWrite;
    try
      VNotifier := FNotifier;
      FNotifier := nil;
    finally
      FCS.EndWrite;
    end;
    if VNotifier <> nil then begin
      VNotifier.Notify(nil);
    end;
  end;
end;

function TNotifierOneOperation.GetIsExecuted: Boolean;
begin
  Result := InterlockedCompareExchange(FExecutedCount, 0, 0) <> 0;
end;

procedure TNotifierOneOperation.RemoveListener(AListener: IListener);
var
  VNotifier: INotifierInternal;
begin
  if not GetIsExecuted then begin
    FCS.BeginRead;
    try
      VNotifier := FNotifier;
    finally
      FCS.EndRead;
    end;
    if VNotifier <> nil then begin
      // sync internally
      VNotifier.Remove(AListener);
    end;
  end;
end;

end.










