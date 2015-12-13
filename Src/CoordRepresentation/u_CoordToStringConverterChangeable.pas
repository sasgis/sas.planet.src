{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_CoordToStringConverterChangeable;

interface

uses
  i_Notifier,
  i_Listener,
  i_CoordToStringConverter,
  i_CoordRepresentationConfig,
  u_ChangeableBase;

type
  TCoordToStringConverterChangeable = class(TChangeableWithSimpleLockBase, ICoordToStringConverterChangeable)
  private
    FConfig: ICoordRepresentationConfig;
    FDependentNotifier: INotifier;
    FDependentListener: IListener;
    FStatic: ICoordToStringConverter;
    procedure OnDependentNotifier;
    function CreateStatic: ICoordToStringConverter;
  private
    function GetStatic: ICoordToStringConverter;
  public
    constructor Create(
      const AConfig: ICoordRepresentationConfig;
      const ADependentNotifier: INotifier
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_ListenerByEvent,
  u_CoordToStringConverter;

{ TCoordToStringConverterChangeable }

constructor TCoordToStringConverterChangeable.Create(
  const AConfig: ICoordRepresentationConfig;
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

destructor TCoordToStringConverterChangeable.Destroy;
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

function TCoordToStringConverterChangeable.CreateStatic: ICoordToStringConverter;
var
  VConfig: ICoordRepresentationConfigStatic;
begin
  VConfig := FConfig.GetStatic;
  Result :=
    TCoordToStringConverter.Create(
      VConfig.IsLatitudeFirst,
      VConfig.DegrShowFormat,
      VConfig.CoordSysType
    );
end;

function TCoordToStringConverterChangeable.GetStatic: ICoordToStringConverter;
begin
  CS.BeginRead;
  try
    Result := FStatic;
  finally
    CS.EndRead;
  end;
end;

procedure TCoordToStringConverterChangeable.OnDependentNotifier;
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
