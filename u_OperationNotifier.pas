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

unit u_OperationNotifier;

interface

uses
  Windows,
  SyncObjs,
  i_JclNotify,
  i_OperationNotifier;

type
  IOperationNotifierInternal = interface
    procedure NextOperation;
  end;

  TOperationNotifier = class(TInterfacedObject, IOperationNotifier, IOperationNotifierInternal)
  private
    FNotifier: IJclNotifier;
    FCurrentOperationID: Integer;
    FCS: TCriticalSection;
  protected
    procedure NextOperation;
  protected
    function GetCurrentOperation: Integer; stdcall;
    function IsOperationCanceled(AID: Integer): Boolean; stdcall;

    procedure AddListener(AListener: IJclListener); stdcall;
    procedure RemoveListener(AListener: IJclListener); stdcall;
  public
    constructor Create();
    destructor Destroy; override;
  end;

implementation

uses
  u_JclNotify;

{ TOperationNotifier }

constructor TOperationNotifier.Create;
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FNotifier := TJclBaseNotifier.Create;
  FCurrentOperationID := 0;
end;

destructor TOperationNotifier.Destroy;
begin
  FCS.Free;
  inherited Destroy;
end;

procedure TOperationNotifier.AddListener(AListener: IJclListener);
begin
  FCS.Acquire;
  try
    FNotifier.Add(AListener);
  finally
    FCS.Release;
  end;
end;

function TOperationNotifier.IsOperationCanceled(AID: Integer): Boolean;
begin
  FCS.Acquire;
  try
    Result := FCurrentOperationID <> AID;
  finally
    FCS.Release;
  end;
end;

function TOperationNotifier.GetCurrentOperation: Integer;
begin
  FCS.Acquire;
  try
    Result := FCurrentOperationID;
  finally
    FCS.Release;
  end;
end;

procedure TOperationNotifier.NextOperation;
begin
  FCS.Acquire;
  try
    Inc(FCurrentOperationID);
  finally
    FCS.Release;
  end;
end;

procedure TOperationNotifier.RemoveListener(AListener: IJclListener);
begin
  FCS.Acquire;
  try
    FNotifier.Remove(AListener);
  finally
    FCS.Release;
  end;
end;

end.
