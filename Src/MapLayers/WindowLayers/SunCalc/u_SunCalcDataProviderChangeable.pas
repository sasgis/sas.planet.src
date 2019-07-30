{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_SunCalcDataProviderChangeable;

interface

uses
  SysUtils,
  i_Listener,
  i_SunCalcConfig,
  i_SunCalcDataProvider,
  u_ConfigDataElementBase;

type
  TSunCalcDataProviderChangeable = class(
    TConfigDataElementWithStaticBaseEmptySaveLoad,
    ISunCalcDataProviderChangeable
  )
  private
    FConfig: ISunCalcConfig;
    FListener: IListener;
    FProviderType: TSunCalcDataProviderType;
    procedure OnConfigChange;
  protected
    function CreateStatic: IInterface; override;
  private
    { ISunCalcDataProviderChangeable }
    function GetStatic: ISunCalcDataProvider;
  public
    constructor Create(const AConfig: ISunCalcConfig);
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_SunCalcDataProviderSun;

{ TSunCalcDataProviderChangeable }

constructor TSunCalcDataProviderChangeable.Create(const AConfig: ISunCalcConfig);
begin
  inherited Create;
  FConfig := AConfig;
  FListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfig.ChangeNotifier.Add(FListener);
end;

destructor TSunCalcDataProviderChangeable.Destroy;
begin
  if (FConfig <> nil) and (FListener <> nil) then begin
    FConfig.ChangeNotifier.Remove(FListener);
    FConfig := nil;
  end;
  inherited Destroy;
end;

function TSunCalcDataProviderChangeable.CreateStatic: IInterface;
begin
  Result := nil;
  case FProviderType of
    scdpSun: Result := TSunCalcDataProviderSun.Create;
    scdpMoon: Assert(False, 'ToDo');
  else
    Assert(False);
  end;
end;

function TSunCalcDataProviderChangeable.GetStatic: ISunCalcDataProvider;
begin
  Result := GetStaticInternal as ISunCalcDataProvider;
end;

procedure TSunCalcDataProviderChangeable.OnConfigChange;
var
  VType: TSunCalcDataProviderType;
begin
  LockWrite;
  try
    VType := FConfig.DataProviderType;
    if VType <> FProviderType then begin
      FProviderType := VType;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
