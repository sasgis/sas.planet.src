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

unit u_BitmapChangeableFaked;

interface

uses
  i_Notifier,
  i_Bitmap32Static,
  u_BaseInterfacedObject;

type
  TBitmapChangeableFaked = class(TBaseInterfacedObject, IBitmapChangeable)
  private
    FBitmap: IBitmap32Static;
    FChangeNotifier: INotifier;
  private
    function GetStatic: IBitmap32Static;
    function GetBeforeChangeNotifier: INotifier;
    function GetChangeNotifier: INotifier;
    function GetAfterChangeNotifier: INotifier;
  public
    constructor Create(const ABitmap: IBitmap32Static);
  end;

implementation

uses
  u_Notifier;

{ TBitmapChangeableFaked }

constructor TBitmapChangeableFaked.Create(const ABitmap: IBitmap32Static);
begin
  inherited Create;
  FBitmap := ABitmap;
  FChangeNotifier := TNotifierFaked.Create;
end;

function TBitmapChangeableFaked.GetAfterChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TBitmapChangeableFaked.GetBeforeChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TBitmapChangeableFaked.GetChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TBitmapChangeableFaked.GetStatic: IBitmap32Static;
begin
  Result := FBitmap;
end;

end.
