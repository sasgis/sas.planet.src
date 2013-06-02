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

unit u_TreeChangeableBase;

interface

uses
  SysUtils,
  i_Notifier,
  i_Listener,
  i_StaticTreeItem,
  i_StaticTreeBuilder,
  i_TreeChangeable,
  u_BaseInterfacedObject;

type
  TTreeChangeableBase = class(TBaseInterfacedObject, ITreeChangeable)
  private
    FStaticTreeBuilder: IStaticTreeBuilder;
    FConfigChangeListener: IListener;
    FConfigChangeNotifier: INotifier;
    FChangeNotifier: INotifierInternal;
    FCS: IReadWriteSync;

    FStatic: IStaticTreeItem;
    procedure OnConfigChange;
  protected
    function CreateStatic: IStaticTreeItem;
    function GetSource: IInterface; virtual; abstract;
  protected
    function GetStatic: IStaticTreeItem;
    function GetChangeNotifier: INotifier;
  public
    constructor Create(
      const AStaticTreeBuilder: IStaticTreeBuilder;
      const AConfigChangeNotifier: INotifier
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  u_Notifier,
  u_ListenerByEvent;

{ TTreeChangeableBase }

constructor TTreeChangeableBase.Create(
  const AStaticTreeBuilder: IStaticTreeBuilder;
  const AConfigChangeNotifier: INotifier
);
begin
  inherited Create;
  FStaticTreeBuilder := AStaticTreeBuilder;
  FConfigChangeNotifier := AConfigChangeNotifier;
  FChangeNotifier := TNotifierBase.Create;
  FCS := MakeSyncRW_Var(Self);
  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfigChangeNotifier.Add(FConfigChangeListener);
  OnConfigChange;
end;

destructor TTreeChangeableBase.Destroy;
begin
  if Assigned(FConfigChangeNotifier) and Assigned(FConfigChangeListener) then begin
    FConfigChangeNotifier.Remove(FConfigChangeListener);
    FConfigChangeListener := nil;
    FConfigChangeNotifier := nil;
  end;

  FCS := nil;

  inherited;
end;

function TTreeChangeableBase.CreateStatic: IStaticTreeItem;
begin
  Result := FStaticTreeBuilder.BuildStatic(GetSource);
end;

function TTreeChangeableBase.GetChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TTreeChangeableBase.GetStatic: IStaticTreeItem;
begin
  FCS.BeginRead;
  try
    Result := FStatic;
  finally
    FCS.EndRead;
  end;
end;

procedure TTreeChangeableBase.OnConfigChange;
var
  VStatic: IStaticTreeItem;
begin
  VStatic := CreateStatic;
  FCS.BeginWrite;
  try
    FStatic := VStatic;
  finally
    FCS.EndWrite;
  end;
  FChangeNotifier.Notify(nil);
end;

end.
