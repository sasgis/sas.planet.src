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

unit u_ThreadCacheManagerAbstract;

interface

uses
  Classes,
  i_Listener,
  i_NotifierOperation;

type
  TThreadCacheManagerAbstract = class(TThread)
  private
    FOperationID: Integer;
    FCancelListener: IListener;
    FCancelNotifier: INotifierOperation;
    FDebugThreadName: AnsiString;
    procedure OnCancel;
  protected
    procedure Process; virtual; abstract;
    procedure Execute; override;
    property CancelNotifier: INotifierOperation read FCancelNotifier;
  public
    constructor Create(
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const ADebugThreadName: AnsiString = ''
    );
    destructor Destroy; override;
  end;

implementation

uses
  Dialogs,
  SysUtils,
  u_ReadableThreadNames,
  u_ListenerByEvent;

{ TThreadCacheManagerAbstract }

constructor TThreadCacheManagerAbstract.Create(
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer;
  const ADebugThreadName: AnsiString = ''
);
begin
  inherited Create(True);
  FDebugThreadName := ADebugThreadName;
  Self.Priority := tpNormal;
  Self.FreeOnTerminate := True;
  FCancelNotifier := ACancelNotifier;
  FOperationID := AOperationID;
  if not FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    FCancelListener := TNotifyNoMmgEventListener.Create(Self.OnCancel);
    FCancelNotifier.AddListener(FCancelListener);
  end;
  if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    Terminate;
  end else begin
    Resume;
  end;
end;

destructor TThreadCacheManagerAbstract.Destroy;
begin
  if Assigned(FCancelNotifier) and Assigned(FCancelListener) then begin
    FCancelNotifier.RemoveListener(FCancelListener);
    FCancelListener := nil;
    FCancelNotifier := nil;
  end;
  inherited;
end;

procedure TThreadCacheManagerAbstract.Execute;
begin
  SetCurrentThreadName(FDebugThreadName);
  Process;
end;

procedure TThreadCacheManagerAbstract.OnCancel;
begin
  Terminate;
end;

end.


