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

unit u_InterfacedThread;

interface

uses
  Classes,
  SyncObjs,
  i_Thread;

type
  TInterfacedThread = class(TInterfacedObject, IThread)
  private
    FThread: TThread;
    FCS: TCriticalSection;
    FTerminated: Boolean;
  protected
    procedure Start; virtual;
    procedure Terminate; virtual;
    function GetPriority: TThreadPriority;
    procedure SetPriority(Value: TThreadPriority);
    function WaitFor: LongWord; virtual;
    procedure Synchronize(AMethod: TThreadMethod);
    procedure Execute; virtual; abstract;
    property Terminated: Boolean read FTerminated;
  public
    constructor Create();
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

type
  TThread4InterfacedThread = class(TThread)
  private
    FRef: IInterface;
    FExec: TThreadMethod;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
  public
    constructor Create(AExec: TThreadMethod);
    procedure Start(ARef: IInterface);
  end;

{ TInterfacedThread }

constructor TInterfacedThread.Create;
begin
  inherited;
  FThread := TThread4InterfacedThread.Create(Self.Execute);
  FTerminated := False;
  FCS := TCriticalSection.Create;
end;

destructor TInterfacedThread.Destroy;
begin
  FreeAndNil(FCS);
  if not FTerminated then begin
    FThread.Terminate;
  end;
  FreeAndNil(FThread);
  inherited;
end;

function TInterfacedThread.GetPriority: TThreadPriority;
begin
  Result := FThread.Priority;
end;

procedure TInterfacedThread.SetPriority(Value: TThreadPriority);
begin
  FThread.Priority := Value;
end;

procedure TInterfacedThread.Start;
begin
  FCS.Acquire;
  try
    if not FTerminated then begin
      TThread4InterfacedThread(FThread).Start(Self);
    end;
  finally
    FCS.Release;
  end;
end;

procedure TInterfacedThread.Synchronize(AMethod: TThreadMethod);
begin
  FThread.Synchronize(FThread, AMethod);
end;

procedure TInterfacedThread.Terminate;
begin
  FCS.Acquire;
  try
    if not FTerminated then begin
      FTerminated := True;
      FThread.Terminate;
    end;
  finally
    FCS.Release;
  end;
end;

function TInterfacedThread.WaitFor: LongWord;
begin
  Result := FThread.WaitFor;
end;

{ TThread4InterfacedThread }

constructor TThread4InterfacedThread.Create(AExec: TThreadMethod);
begin
  inherited Create(True);
  FExec := AExec;
end;

procedure TThread4InterfacedThread.DoTerminate;
begin
  inherited;
  FRef := nil;
end;

procedure TThread4InterfacedThread.Execute;
begin
  inherited;
  FExec;
end;

procedure TThread4InterfacedThread.Start(ARef: IInterface);
begin
  FRef := ARef;
  if not Terminated then begin
    Resume;
  end;
end;

end.
