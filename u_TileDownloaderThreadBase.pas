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

unit u_TileDownloaderThreadBase;

interface

uses
  Windows,
  Classes,
  SyncObjs,
  Types,
  i_OperationNotifier,
  u_OperationNotifier,
  u_MapType;

type
  TTileDownloaderThreadBase = class(TThread)
  private
    FCancelNotifierInternal: IOperationNotifierInternal;
  protected
    FMapType: TMapType;
    FZoom: byte;
    FLoadUrl: string;
    FCancelEvent: TEvent;
    FCancelNotifier: IOperationNotifier;

    procedure SleepCancelable(ATime: Cardinal);
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Terminate; reintroduce;
    property MapType: TMapType read FMapType;
  end;

implementation

uses
  SysUtils;

constructor TTileDownloaderThreadBase.Create(CreateSuspended: Boolean);
var
  VOperationNotifier: TOperationNotifier;
begin
  inherited Create(CreateSuspended);
  FCancelEvent := TEvent.Create;
  VOperationNotifier := TOperationNotifier.Create;
  FCancelNotifierInternal := VOperationNotifier;
  FCancelNotifier := VOperationNotifier;
end;

destructor TTileDownloaderThreadBase.Destroy;
begin
  Terminate;
  inherited;
  FreeAndNil(FCancelEvent);
end;

procedure TTileDownloaderThreadBase.SleepCancelable(ATime: Cardinal);
begin
  if  ATime > 0 then begin
    FCancelEvent.WaitFor(ATime);
  end;
end;

procedure TTileDownloaderThreadBase.Terminate;
begin
  inherited;
  FCancelEvent.SetEvent;
  FCancelNotifierInternal.NextOperation;
end;

end.
