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

unit u_BitmapMarkerProviderChangeableFaked;

interface

uses
  i_JclNotify,
  i_BitmapMarker;

type
  TBitmapMarkerProviderChangeableFaked = class(TInterfacedObject, IBitmapMarkerProviderChangeable)
  private
    FProviderStatic: IBitmapMarkerProvider;
    FChangeNotifier: IJclNotifier;
  protected
    function GetStatic: IBitmapMarkerProvider;
    function GetChangeNotifier: IJclNotifier;
  public
    constructor Create(
      AProviderStatic: IBitmapMarkerProvider
    );
  end;

implementation

uses
  u_JclNotify;

{ TBitmapMarkerProviderChangeableFaked }

constructor TBitmapMarkerProviderChangeableFaked.Create(
  AProviderStatic: IBitmapMarkerProvider);
begin
  FProviderStatic := AProviderStatic;
  FChangeNotifier := TJclBaseNotifierFaked.Create;
end;

function TBitmapMarkerProviderChangeableFaked.GetChangeNotifier: IJclNotifier;
begin
  Result := FChangeNotifier;
end;

function TBitmapMarkerProviderChangeableFaked.GetStatic: IBitmapMarkerProvider;
begin
  Result := FProviderStatic;
end;

end.
