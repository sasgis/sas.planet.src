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

unit u_NotifierOperation;

interface

uses
  Windows,
  SysUtils,
  i_Notifier,
  i_Listener,
  i_NotifierOperation,
  u_BaseInterfacedObject;

type
  TNotifierOperation = class(TBaseInterfacedObject, INotifierOperation, INotifierOperationInternal)
  private
    FNotifier: INotifierInternal;
    FCurrentOperationID: Integer;
  private
    procedure NextOperation(const AMsg: IInterface = nil);
  private
    function GetCurrentOperation: Integer;
    function IsOperationCanceled(AID: Integer): Boolean;

    procedure AddListener(const AListener: IListener);
    procedure RemoveListener(const AListener: IListener);
  public
    constructor Create(const ANotifier: INotifierInternal);
  end;

  TNotifierOneOperation = class(TBaseInterfacedObject, INotifier, INotifierOneOperation, INotifierOneOperationInternal)
  private
    FExecutedCount: Integer;
    FNotifier: INotifierInternal;
    FCS: IReadWriteSync;
  private
    procedure ExecuteOperation(const AMsg: IInterface = nil);
  private
    function GetIsExecuted: Boolean;

    procedure Add(const AListener: IListener);
    procedure Remove(const AListener: IListener);
  public
    constructor Create(const ANotifier: INotifierInternal);
  end;

  TNotifierOneOperationByNotifier = class(TBaseInterfacedObject, INotifier, INotifierOneOperation)
  private
    FSourceNotifier: INotifierOperation;
    FSourceID: Integer;
  private
    function GetIsExecuted: Boolean;

    procedure Add(const AListener: IListener);
    procedure Remove(const AListener: IListener);
  public
    constructor Create(
      const ASourceNotifier: INotifierOperation;
      ASourceID: Integer
    );
  end;

implementation

uses
  u_Synchronizer;

{ TNotifierOperation }

constructor TNotifierOperation.Create(const ANotifier: INotifierInternal);
begin
  Assert(ANotifier <> nil);
  inherited Create;
  FNotifier := ANotifier;
  FCurrentOperationID := 0;
end;

procedure TNotifierOperation.AddListener(const AListener: IListener);
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

procedure TNotifierOperation.NextOperation(const AMsg: IInterface);
begin
  InterlockedIncrement(FCurrentOperationID);
  FNotifier.Notify(AMsg);
end;

procedure TNotifierOperation.RemoveListener(const AListener: IListener);
begin
  FNotifier.Remove(AListener);
end;

{ TNotifierOneOperation }

constructor TNotifierOneOperation.Create(const ANotifier: INotifierInternal);
begin
  Assert(ANotifier <> nil);
  inherited Create;
  FNotifier := ANotifier;
  FCS := MakeSyncRW_Var(Self, False);
  FExecutedCount := 0;
end;

procedure TNotifierOneOperation.Add(const AListener: IListener);
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

procedure TNotifierOneOperation.ExecuteOperation(const AMsg: IInterface);
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
      VNotifier.Notify(AMsg);
    end;
  end;
end;

function TNotifierOneOperation.GetIsExecuted: Boolean;
begin
  Result := InterlockedCompareExchange(FExecutedCount, 0, 0) <> 0;
end;

procedure TNotifierOneOperation.Remove(const AListener: IListener);
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

{ TNotifierOneOperationByNotifier }

constructor TNotifierOneOperationByNotifier.Create(
  const ASourceNotifier: INotifierOperation; ASourceID: Integer);
begin
  inherited Create;
  FSourceNotifier := ASourceNotifier;
  FSourceID := ASourceID;
end;

procedure TNotifierOneOperationByNotifier.Add(const AListener: IListener);
begin
  FSourceNotifier.AddListener(AListener);
end;

function TNotifierOneOperationByNotifier.GetIsExecuted: Boolean;
begin
  Result := FSourceNotifier.IsOperationCanceled(FSourceID);
end;

procedure TNotifierOneOperationByNotifier.Remove(const AListener: IListener);
begin
  FSourceNotifier.RemoveListener(AListener);
end;

end.










