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

unit u_MarkSystemImplFactoryChangeable;

interface

uses
  i_Notifier,
  i_MarkSystemImplFactory,
  u_BaseInterfacedObject;

type
  TMarkSystemImplFactoryChangeableFaked = class(TBaseInterfacedObject, IMarkSystemImplFactoryChangeable)
  private
    FFactory: IMarkSystemImplFactory;
    FChangeNotifier: INotifier;
  private
    function GetStatic: IMarkSystemImplFactory;
    function GetBeforeChangeNotifier: INotifier;
    function GetChangeNotifier: INotifier;
    function GetAfterChangeNotifier: INotifier;
  public
    constructor Create(const AFactory: IMarkSystemImplFactory);
  end;

implementation

uses
  u_Notifier;

{ TMarkSystemImplFactoryChangeableFaked }

constructor TMarkSystemImplFactoryChangeableFaked.Create(
  const AFactory: IMarkSystemImplFactory
);
begin
  inherited Create;
  FFactory := AFactory;
  FChangeNotifier := TNotifierFaked.Create;
end;

function TMarkSystemImplFactoryChangeableFaked.GetAfterChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TMarkSystemImplFactoryChangeableFaked.GetBeforeChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TMarkSystemImplFactoryChangeableFaked.GetChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TMarkSystemImplFactoryChangeableFaked.GetStatic: IMarkSystemImplFactory;
begin
  Result := FFactory;
end;

end.
