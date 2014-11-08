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

unit u_SystemTimeProvider;

interface

uses
  Windows,
  i_Notifier,
  i_SystemTimeProvider,
  u_BaseInterfacedObject;

type
  TSystemTimeProvider = class(TBaseInterfacedObject, ISystemTimeProvider, ISystemTimeProviderInternal)
  private
    FSystemTimeChangedNotifier: INotifierInternal;
    FLocalTimeShift: Double;
  private
    function GetLocalTime: TDateTime;
    function GetUTCTime: TDateTime;
    function UTCToLocalTime(const ASysTime: TDateTime): TDateTime;
    function GetSystemTimeChangedNotifier: INotifier;
  private
    procedure SystemTimeChanged;
  public
    constructor Create();
  end;

implementation

uses
  SysUtils,
  u_Notifier,
  u_Synchronizer;

{ TSystemTimeProvider }

constructor TSystemTimeProvider.Create;
begin
  inherited Create;
  FSystemTimeChangedNotifier :=
    TNotifierBase.Create(
      GSync.SyncVariable.Make(Self.ClassName)
    );
  SystemTimeChanged;
end;

function TSystemTimeProvider.GetLocalTime: TDateTime;
begin
  Result := GetUTCTime + FLocalTimeShift;
end;

function TSystemTimeProvider.GetSystemTimeChangedNotifier: INotifier;
begin
  Result := FSystemTimeChangedNotifier;
end;

function TSystemTimeProvider.GetUTCTime: TDateTime;
var
  st: TSystemTime;
begin
  GetSystemTime(st);
  Result := SystemTimeToDateTime(st);
end;

procedure TSystemTimeProvider.SystemTimeChanged;
var
  st, lt: TSystemTime;
begin
  FLocalTimeShift := 0;
  GetSystemTime(st);
  if SystemTimeToTzSpecificLocalTime(nil, st, lt) then begin
    FLocalTimeShift := (SystemTimeToDateTime(lt) - SystemTimeToDateTime(st));
  end;
  FSystemTimeChangedNotifier.Notify(nil);
end;

function TSystemTimeProvider.UTCToLocalTime(
  const ASysTime: TDateTime): TDateTime;
begin
  if (0 = ASysTime) then begin
    Result := 0;
  end else begin
    Result := ASysTime + FLocalTimeShift;
  end;
end;

end.
