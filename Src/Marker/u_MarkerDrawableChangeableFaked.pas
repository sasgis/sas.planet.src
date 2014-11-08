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

unit u_MarkerDrawableChangeableFaked;

interface

uses
  i_Notifier,
  i_MarkerDrawable,
  u_BaseInterfacedObject;

type
  TMarkerDrawableChangeableFaked = class(TBaseInterfacedObject, IMarkerDrawableChangeable)
  private
    FMarker: IMarkerDrawable;
    FChangeNotifier: INotifier;
  private
    function GetStatic: IMarkerDrawable;
    function GetBeforeChangeNotifier: INotifier;
    function GetChangeNotifier: INotifier;
    function GetAfterChangeNotifier: INotifier;
  public
    constructor Create(const AMarker: IMarkerDrawable);
  end;

type
  TMarkerDrawableWithDirectionChangeableFaked = class(TBaseInterfacedObject, IMarkerDrawableWithDirectionChangeable)
  private
    FMarker: IMarkerDrawableWithDirection;
    FChangeNotifier: INotifier;
  private
    function GetStatic: IMarkerDrawableWithDirection;
    function GetBeforeChangeNotifier: INotifier;
    function GetChangeNotifier: INotifier;
    function GetAfterChangeNotifier: INotifier;
  public
    constructor Create(const AMarker: IMarkerDrawableWithDirection);
  end;

implementation

uses
  u_Notifier;

{ TMarkerDrawableChangeableFaked }

constructor TMarkerDrawableChangeableFaked.Create(
  const AMarker: IMarkerDrawable);
begin
  inherited Create;
  FMarker := AMarker;
  FChangeNotifier := TNotifierFaked.Create;
end;

function TMarkerDrawableChangeableFaked.GetAfterChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TMarkerDrawableChangeableFaked.GetBeforeChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TMarkerDrawableChangeableFaked.GetChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TMarkerDrawableChangeableFaked.GetStatic: IMarkerDrawable;
begin
  Result := FMarker;
end;

{ TMarkerDrawableWithDirectionChangeableFaked }

constructor TMarkerDrawableWithDirectionChangeableFaked.Create(
  const AMarker: IMarkerDrawableWithDirection);
begin
  inherited Create;
  FMarker := AMarker;
  FChangeNotifier := TNotifierFaked.Create;
end;

function TMarkerDrawableWithDirectionChangeableFaked.GetAfterChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TMarkerDrawableWithDirectionChangeableFaked.GetBeforeChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TMarkerDrawableWithDirectionChangeableFaked.GetChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TMarkerDrawableWithDirectionChangeableFaked.GetStatic: IMarkerDrawableWithDirection;
begin
  Result := FMarker;
end;

end.
