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

unit u_ActiveMapTBXItem;

interface

uses
  Graphics,
  Classes,
  TB2Item,
  TBX,
  i_Listener,
  i_ActiveMapsConfig;

type
  TActiveMapTBXItem = class(TTBXCustomItem)
  private
    FGUID: TGUID;
    FConfig: IActiveMapConfig;
    FListener: IListener;
    procedure OnMapChangeState;
    procedure AdjustFont(
      Item: TTBCustomItem;
      Viewer: TTBItemViewer;
      Font: TFont;
      StateFlags: Integer
    );
    procedure ItemClick(Sender: TObject);
  public
    constructor Create(
      AOwner: TComponent;
      const AGUID: TGUID;
      const AConfig: IActiveMapConfig
    ); reintroduce;
    destructor Destroy; override;
  end;

  TActiveLayerTBXItem = class(TTBXCustomItem)
  private
    FGUID: TGUID;
    FConfig: IActiveLayersConfig;
    FListener: IListener;
    procedure OnMapChangeState;
    procedure AdjustFont(
      Item: TTBCustomItem;
      Viewer: TTBItemViewer;
      Font: TFont;
      StateFlags: Integer
    );
    procedure ItemClick(Sender: TObject);
  public
    constructor Create(
      AOwner: TComponent;
      const AGUID: TGUID;
      const AConfig: IActiveLayersConfig
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_GUIDListStatic,
  u_ListenerByEvent;

{ TActiveMapTBXItem }

constructor TActiveMapTBXItem.Create(
  AOwner: TComponent;
  const AGUID: TGUID;
  const AConfig: IActiveMapConfig
);
begin
  Assert(Assigned(AConfig));
  inherited Create(AOwner);
  FGUID := AGUID;
  FConfig := AConfig;
  OnAdjustFont := Self.AdjustFont;
  OnClick := Self.ItemClick;
  FListener := TNotifyNoMmgEventListener.Create(Self.OnMapChangeState);
  FConfig.ChangeNotifier.Add(FListener);
  OnMapChangeState;
end;

destructor TActiveMapTBXItem.Destroy;
begin
  if Assigned(FConfig) and Assigned(FListener) then begin
    FConfig.ChangeNotifier.Remove(FListener);
    FListener := nil;
  end;
  inherited;
end;

procedure TActiveMapTBXItem.AdjustFont(
  Item: TTBCustomItem;
  Viewer: TTBItemViewer;
  Font: TFont;
  StateFlags: Integer
);
begin
  if Self.Checked then begin
    Self.FontSettings.Bold := tsTrue;
  end else begin
    Self.FontSettings.Bold := tsDefault;
  end;
end;

procedure TActiveMapTBXItem.ItemClick(Sender: TObject);
begin
  FConfig.MainMapGUID := FGUID;
end;

procedure TActiveMapTBXItem.OnMapChangeState;
begin
  Self.Checked := IsEqualGUID(FConfig.MainMapGUID, FGUID);
end;

{ TActiveLayerTBXItem }

constructor TActiveLayerTBXItem.Create(
  AOwner: TComponent;
  const AGUID: TGUID;
  const AConfig: IActiveLayersConfig
);
begin
  Assert(Assigned(AConfig));
  inherited Create(AOwner);
  FGUID := AGUID;
  FConfig := AConfig;
  OnAdjustFont := Self.AdjustFont;
  OnClick := Self.ItemClick;
  FListener := TNotifyNoMmgEventListener.Create(Self.OnMapChangeState);
  FConfig.ChangeNotifier.Add(FListener);
  OnMapChangeState;
end;

destructor TActiveLayerTBXItem.Destroy;
begin
  if Assigned(FConfig) and Assigned(FListener) then begin
    FConfig.ChangeNotifier.Remove(FListener);
    FListener := nil;
  end;
  inherited;
end;

procedure TActiveLayerTBXItem.AdjustFont(
  Item: TTBCustomItem;
  Viewer: TTBItemViewer;
  Font: TFont;
  StateFlags: Integer
);
begin
  if Self.Checked then begin
    Self.FontSettings.Bold := tsTrue;
  end else begin
    Self.FontSettings.Bold := tsDefault;
  end;
end;

procedure TActiveLayerTBXItem.ItemClick(Sender: TObject);
begin
  FConfig.InvertLayerSelectionByGUID(FGUID);
end;

procedure TActiveLayerTBXItem.OnMapChangeState;
var
  VLayers: IGUIDSetStatic;
begin
  VLayers := FConfig.LayerGuids;
  Self.Checked := Assigned(VLayers) and VLayers.IsExists(FGUID);
end;

end.
