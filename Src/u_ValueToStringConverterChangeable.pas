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

unit u_ValueToStringConverterChangeable;

interface

uses
  i_Notifier,
  i_Listener,
  i_ValueToStringConverter,
  i_ValueToStringConverterConfig,
  u_ChangeableBase;

type
  TValueToStringConverterChangeable = class(TChangeableWithSimpleLockBase, IValueToStringConverterChangeable)
  private
    FConfig: IValueToStringConverterConfig;
    FDependentNotifier: INotifier;
    FDependentListener: IListener;
    FStatic: IValueToStringConverter;
    procedure OnDependentNotifier;
    function CreateStatic: IValueToStringConverter;
  private
    function GetStatic: IValueToStringConverter;
  public
    constructor Create(
      const AConfig: IValueToStringConverterConfig;
      const ADependentNotifier: INotifier
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_ListenerByEvent,
  u_ValueToStringConverter;

{ TValueToStringConverterChangeable }

constructor TValueToStringConverterChangeable.Create(
  const AConfig: IValueToStringConverterConfig;
  const ADependentNotifier: INotifier
);
begin
  inherited Create;
  FConfig := AConfig;
  FDependentNotifier := ADependentNotifier;
  FDependentListener := TNotifyNoMmgEventListener.Create(Self.OnDependentNotifier);
  FDependentNotifier.Add(FDependentListener);
  FConfig.ChangeNotifier.Add(FDependentListener);
  FStatic := CreateStatic;
end;

destructor TValueToStringConverterChangeable.Destroy;
begin
  if Assigned(FDependentNotifier) and Assigned(FDependentListener) then begin
    FDependentNotifier.Remove(FDependentListener);
    FDependentNotifier := nil;
  end;
  if Assigned(FConfig) and Assigned(FDependentListener) then begin
    FConfig.ChangeNotifier.Remove(FDependentListener);
    FConfig := nil;
  end;
  inherited;
end;

function TValueToStringConverterChangeable.CreateStatic: IValueToStringConverter;
var
  VConfig: IValueToStringConverterConfigStatic;
begin
  VConfig := FConfig.GetStatic;
  Result :=
    TValueToStringConverter.Create(
      VConfig.DistStrFormat,
      VConfig.IsLatitudeFirst,
      VConfig.DegrShowFormat,
      VConfig.AreaShowFormat
    );
end;

function TValueToStringConverterChangeable.GetStatic: IValueToStringConverter;
begin
  CS.BeginRead;
  try
    Result := FStatic;
  finally
    CS.EndRead;
  end;
end;

procedure TValueToStringConverterChangeable.OnDependentNotifier;
begin
  CS.BeginWrite;
  try
    FStatic := CreateStatic;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

end.
