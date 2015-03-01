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

unit u_TreeChangeableBase;

interface

uses
  SysUtils,
  i_Notifier,
  i_Listener,
  i_StaticTreeItem,
  i_TreeChangeable,
  u_ChangeableBase;

type
  TTreeChangeableBase = class(TChangeableWithSimpleLockBase, ITreeChangeable)
  private
    FConfigChangeListener: IListener;
    FConfigChangeNotifier: INotifier;

    FStatic: IStaticTreeItem;
    procedure OnConfigChange;
  protected
    function CreateStatic: IStaticTreeItem; virtual; abstract;
  protected
    function GetStatic: IStaticTreeItem;
  public
    procedure AfterConstruction; override;
    constructor Create(
      const AConfigChangeNotifier: INotifier
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent;

{ TTreeChangeableBase }

constructor TTreeChangeableBase.Create(
  const AConfigChangeNotifier: INotifier
);
begin
  inherited Create;
  FConfigChangeNotifier := AConfigChangeNotifier;
  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfigChangeNotifier.Add(FConfigChangeListener);
end;

procedure TTreeChangeableBase.AfterConstruction;
begin
  inherited;
  OnConfigChange;
end;

destructor TTreeChangeableBase.Destroy;
begin
  if Assigned(FConfigChangeNotifier) and Assigned(FConfigChangeListener) then begin
    FConfigChangeNotifier.Remove(FConfigChangeListener);
    FConfigChangeListener := nil;
    FConfigChangeNotifier := nil;
  end;

  inherited;
end;

function TTreeChangeableBase.GetStatic: IStaticTreeItem;
begin
  CS.BeginRead;
  try
    Result := FStatic;
  finally
    CS.EndRead;
  end;
end;

procedure TTreeChangeableBase.OnConfigChange;
var
  VStatic: IStaticTreeItem;
begin
  VStatic := CreateStatic;
  CS.BeginWrite;
  try
    FStatic := VStatic;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

end.
