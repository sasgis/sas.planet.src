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
  i_MapType,
  i_MapTypeSetChangeable;

type
  TActiveMapTBXItem = class(TTBXCustomItem)
  private
    FMapType: IMapType;
    FActiveMap: IMapTypeChangeable;
    FListener: IListener;
    procedure OnMapChangeState;
    procedure AdjustFont(
      Item: TTBCustomItem;
      Viewer: TTBItemViewer;
      Font: TFont;
      StateFlags: Integer
    );
  public
    constructor Create(
      AOwner: TComponent;
      const AMapType: IMapType;
      const AActiveMap: IMapTypeChangeable
    ); reintroduce;
    destructor Destroy; override;
  end;

  TActiveLayerTBXItem = class(TTBXCustomItem)
  private
    FMapType: IMapType;
    FActiveLayers: IMapTypeSetChangeable;
    FListener: IListener;
    procedure OnMapChangeState;
    procedure AdjustFont(
      Item: TTBCustomItem;
      Viewer: TTBItemViewer;
      Font: TFont;
      StateFlags: Integer
    );
  public
    constructor Create(
      AOwner: TComponent;
      const AMapType: IMapType;
      const AActiveLayers: IMapTypeSetChangeable
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent;

{ TActiveMapTBXItem }

constructor TActiveMapTBXItem.Create(
  AOwner: TComponent;
  const AMapType: IMapType;
  const AActiveMap: IMapTypeChangeable
);
begin
  Assert(Assigned(AActiveMap));
  inherited Create(AOwner);
  FMapType := AMapType;
  FActiveMap := AActiveMap;
  OnAdjustFont := Self.AdjustFont;
  FListener := TNotifyNoMmgEventListener.Create(Self.OnMapChangeState);
  FActiveMap.ChangeNotifier.Add(FListener);
  OnMapChangeState;
end;

destructor TActiveMapTBXItem.Destroy;
begin
  if Assigned(FActiveMap) and Assigned(FListener) then begin
    FActiveMap.ChangeNotifier.Remove(FListener);
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

procedure TActiveMapTBXItem.OnMapChangeState;
begin
  Self.Checked := FActiveMap.GetStatic = FMapType;
end;

{ TActiveLayerTBXItem }

constructor TActiveLayerTBXItem.Create(
  AOwner: TComponent;
  const AMapType: IMapType;
  const AActiveLayers: IMapTypeSetChangeable
);
begin
  Assert(Assigned(AMapType));
  Assert(Assigned(AActiveLayers));
  inherited Create(AOwner);
  FMapType := AMapType;
  FActiveLayers := AActiveLayers;
  OnAdjustFont := Self.AdjustFont;
  FListener := TNotifyNoMmgEventListener.Create(Self.OnMapChangeState);
  FActiveLayers.GetChangeNotifier.Add(FListener);
  OnMapChangeState;
end;

destructor TActiveLayerTBXItem.Destroy;
begin
  if Assigned(FActiveLayers) and Assigned(FListener) then begin
    FActiveLayers.ChangeNotifier.Remove(FListener);
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

procedure TActiveLayerTBXItem.OnMapChangeState;
begin
  Self.Checked := FActiveLayers.GetStatic.IsExists(FMapType.GUID);
end;

end.
