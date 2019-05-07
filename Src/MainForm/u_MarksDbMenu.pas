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

unit u_MarksDbMenu;

interface

uses
  Classes,
  TBX,
  i_Listener,
  i_MarkSystemConfig;

type
  TMarksDbMenu = class(TComponent)
  private
    FSubmenuItem: TTBXSubmenuItem;
    FConfig: IMarkSystemConfigListChangeable;
    FListener: IListener;
    procedure OnConfigChange;
    procedure OnItemClick(Sender: TObject);
  public
    constructor Create(
      const AOwner: TComponent;
      const ASubmenuItem: TTBXSubmenuItem;
      const AConfig: IMarkSystemConfigListChangeable
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  u_MarksExplorerHelper,
  u_ListenerByEvent;

{ TMarksDbMenu }

constructor TMarksDbMenu.Create(
  const AOwner: TComponent;
  const ASubmenuItem: TTBXSubmenuItem;
  const AConfig: IMarkSystemConfigListChangeable
);
begin
  Assert(Assigned(AConfig));

  inherited Create(AOwner);

  FSubmenuItem := ASubmenuItem;
  FConfig := AConfig;

  FListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfig.GetChangeNotifier.Add(FListener);

  OnConfigChange;
end;

destructor TMarksDbMenu.Destroy;
begin
  if Assigned(FConfig) and Assigned(FListener) then begin
    FConfig.GetChangeNotifier.Remove(FListener);
    FListener := nil;
    FConfig := nil;
  end;
  inherited;
end;

procedure TMarksDbMenu.OnItemClick(Sender: TObject);
begin
  Assert(Assigned(Sender));
  FConfig.ActiveConfigID := TComponent(Sender).Tag;
end;

procedure TMarksDbMenu.OnConfigChange;
begin
  RefreshConfigListMenu(FSubmenuItem, False, Self.OnItemClick, FConfig);
end;

end.
