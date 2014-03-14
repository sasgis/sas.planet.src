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

unit u_ChangeableBase;

interface

uses
  i_Notifier,
  i_Changeable,
  u_BaseInterfacedObject;

type
  TChangeableBase = class(TBaseInterfacedObject, IChangeable)
  private
    FBeforeChangeNotifier: INotifierInternal;
    FChangeNotifier: INotifierInternal;
    FAfterChangeNotifier: INotifierInternal;
  protected
    function GetBeforeChangeNotifier: INotifier;
    function GetChangeNotifier: INotifier;
    function GetAfterChangeNotifier: INotifier;
  protected
    procedure DoBeforeChangeNotify; virtual;
    procedure DoInChangeNotify; virtual;
    procedure DoAfterChangeNotify; virtual;

    procedure DoChangeNotify; virtual;
  public
    constructor Create;
  end;

implementation

uses
  u_Notifier;

{ TChangeableBase }

constructor TChangeableBase.Create;
begin
  inherited Create;
  FBeforeChangeNotifier := TNotifierBase.Create;
  FChangeNotifier := TNotifierBase.Create;
  FAfterChangeNotifier := TNotifierBase.Create;
end;

procedure TChangeableBase.DoAfterChangeNotify;
begin
  FAfterChangeNotifier.Notify(nil);
end;

procedure TChangeableBase.DoBeforeChangeNotify;
begin
  FBeforeChangeNotifier.Notify(nil);
end;

procedure TChangeableBase.DoChangeNotify;
begin
  DoBeforeChangeNotify;
  try
    DoInChangeNotify;
  finally
    DoAfterChangeNotify;
  end;
end;

procedure TChangeableBase.DoInChangeNotify;
begin
  FChangeNotifier.Notify(nil);
end;

function TChangeableBase.GetAfterChangeNotifier: INotifier;
begin
  Result := FAfterChangeNotifier;
end;

function TChangeableBase.GetBeforeChangeNotifier: INotifier;
begin
  Result := FBeforeChangeNotifier;
end;

function TChangeableBase.GetChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

end.
