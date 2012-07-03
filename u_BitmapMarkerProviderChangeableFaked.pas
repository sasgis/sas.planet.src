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
  i_Notify,
  i_BitmapMarker;

type
  TBitmapMarkerProviderChangeableFaked = class(TInterfacedObject, IBitmapMarkerProviderChangeable)
  private
    FProviderStatic: IBitmapMarkerProvider;
    FChangeNotifier: INotifier;
  protected
    function GetStatic: IBitmapMarkerProvider;
    function GetChangeNotifier: INotifier;
  public
    constructor Create(
      const AProviderStatic: IBitmapMarkerProvider
    );
  end;

implementation

uses
  u_Notifier;

{ TBitmapMarkerProviderChangeableFaked }

constructor TBitmapMarkerProviderChangeableFaked.Create(
  const AProviderStatic: IBitmapMarkerProvider
);
begin
  inherited Create;
  FProviderStatic := AProviderStatic;
  FChangeNotifier := TBaseNotifierFaked.Create;
end;

function TBitmapMarkerProviderChangeableFaked.GetChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TBitmapMarkerProviderChangeableFaked.GetStatic: IBitmapMarkerProvider;
begin
  Result := FProviderStatic;
end;

end.


