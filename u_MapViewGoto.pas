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

unit u_MapViewGoto;

interface

uses
  t_GeoTypes,
  i_JclNotify,
  i_ViewPortState,
  i_MapViewGoto;

type
  TGotoPosStatic = class(TInterfacedObject, IGotoPosStatic)
  private
    FLonLat: TDoublePoint;
    FZoom: Byte;
    FGotoTime: TDateTime;
  protected
    function GetLonLat: TDoublePoint;
    function GetZoom: Byte;
    function GetGotoTime: TDateTime;
  public
    constructor Create(
      ALonLat: TDoublePoint;
      AZoom: Byte;
      AGotoTime: TDateTime
    );
  end;

  TMapViewGoto = class(TInterfacedObject, IMapViewGoto)
  private
    FViewPortState: IViewPortState;
    FLastGotoPos: IGotoPosStatic;
    FChangeNotifier: IJclNotifier;
  protected
    procedure GotoPos(const ALonLat: TDoublePoint; const AZoom: Byte);
    function GetLastGotoPos: IGotoPosStatic;
    function GetChangeNotifier: IJclNotifier;
  public
    constructor Create(const AViewPortState: IViewPortState);
  end;

implementation

uses
  Math,
  SysUtils,
  u_JclNotify,
  u_GeoFun;

{ TMapViewGoto }

constructor TMapViewGoto.Create(const AViewPortState: IViewPortState);
begin
  inherited Create;
  FViewPortState := AViewPortState;
  FChangeNotifier := TJclBaseNotifier.Create;
  FLastGotoPos := TGotoPosStatic.Create(CEmptyDoublePoint, 0, NaN);
end;

function TMapViewGoto.GetChangeNotifier: IJclNotifier;
begin
  Result := FChangeNotifier;
end;

function TMapViewGoto.GetLastGotoPos: IGotoPosStatic;
begin
  Result := FLastGotoPos;
end;

procedure TMapViewGoto.GotoPos(const ALonLat: TDoublePoint; const AZoom: Byte);
begin
  FViewPortState.LockWrite;
  try
    FLastGotoPos := TGotoPosStatic.Create(ALonLat, AZoom, Now);
    FViewPortState.ChangeZoomWithFreezeAtCenter(AZoom);
    FViewPortState.ChangeLonLat(ALonLat);
  finally
    FViewPortState.UnlockWrite;
  end;
  FChangeNotifier.Notify(nil);
end;

{ TGotoPosStatic }

constructor TGotoPosStatic.Create(ALonLat: TDoublePoint; AZoom: Byte;
  AGotoTime: TDateTime);
begin
  FLonLat := ALonLat;
  FZoom := AZoom;
  FGotoTime := AGotoTime;
end;

function TGotoPosStatic.GetGotoTime: TDateTime;
begin
  Result := FGotoTime;
end;

function TGotoPosStatic.GetLonLat: TDoublePoint;
begin
  Result := FLonLat;
end;

function TGotoPosStatic.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
