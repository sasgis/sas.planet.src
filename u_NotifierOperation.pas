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
  SysUtils,
  i_Notify,
  i_NotifierOperation;

type
  INotifierOperationInternal = interface(INotifierOperation)
    procedure NextOperation;
  end;

  TNotifierOperation = class(TInterfacedObject, INotifierOperation, INotifierOperationInternal)
  private
    FNotifier: INotifierInternal;
    FCurrentOperationID: Integer;
    FCS: IReadWriteSync;
  protected
    procedure NextOperation;
  protected
    function GetCurrentOperation: Integer; stdcall;
    function IsOperationCanceled(AID: Integer): Boolean; stdcall;

    procedure AddListener(AListener: IListener); stdcall;
    procedure RemoveListener(AListener: IListener); stdcall;
  public
    constructor Create;
  end;

  INotifierOneOperationInternal = interface(INotifierOneOperation)
    procedure ExecuteOperation;
  end;

  TNotifierOneOperation = class(TInterfacedObject, INotifierOneOperation, INotifierOneOperationInternal)
  private
    FNotifier: INotifierInternal;
    FCS: IReadWriteSync;
  protected
    procedure ExecuteOperation;
  protected
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
  FCS := MakeSyncRW_Std(Self, TRUE);
  FNotifier := TNotifierBase.Create;
  FCurrentOperationID := 0;
end;

procedure TNotifierOperation.AddListener(AListener: IListener);
begin
  FCS.BeginRead;
  try
    // sync internally
    FNotifier.Add(AListener);
  finally
    FCS.EndRead;
  end;
end;

function TNotifierOperation.IsOperationCanceled(AID: Integer): Boolean;
begin
  FCS.BeginRead;
  try
    Result := FCurrentOperationID <> AID;
  finally
    FCS.EndRead;
  end;
end;

function TNotifierOperation.GetCurrentOperation: Integer;
begin
  FCS.BeginRead;
  try
    Result := FCurrentOperationID;
  finally
    FCS.EndRead;
  end;
end;

procedure TNotifierOperation.NextOperation;
begin
  FCS.BeginWrite;
  try
    Inc(FCurrentOperationID);
    FNotifier.Notify(nil);
  finally
    FCS.EndWrite;
  end;
end;

procedure TNotifierOperation.RemoveListener(AListener: IListener);
begin
  FCS.BeginRead;
  try
    // sync internally
    FNotifier.Remove(AListener);
  finally
    FCS.EndRead;
  end;
end;

{ TNotifierOneOperation }

constructor TNotifierOneOperation.Create;
begin
  FCS := MakeSyncRW_Std(Self, TRUE);
  FNotifier := TNotifierBase.Create;
end;

procedure TNotifierOneOperation.AddListener(AListener: IListener);
begin
  FCS.BeginRead;
  try
    if FNotifier <> nil then begin
      // sync internally
      FNotifier.Add(AListener);
    end;
  finally
    FCS.EndRead;
  end;
end;

procedure TNotifierOneOperation.ExecuteOperation;
var
  VNotifier: INotifierInternal;
begin
  FCS.BeginWrite;
  try
    VNotifier := FNotifier;
    FNotifier := nil;
  finally
    FCS.EndWrite;
  end;
  VNotifier.Notify(nil);
end;

function TNotifierOneOperation.GetIsExecuted: Boolean;
begin
  FCS.BeginRead;
  try
    Result := FNotifier = nil;
  finally
    FCS.EndRead;
  end;
end;

procedure TNotifierOneOperation.RemoveListener(AListener: IListener);
begin
  FCS.BeginRead;
  try
    if FNotifier <> nil then begin
      // sync internally
      FNotifier.Remove(AListener);
    end;
  finally
    FCS.EndRead;
  end;
end;

end.









