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

unit u_ActiveMapTBXItem;

interface

uses
  Graphics,
  Classes,
  TB2Item,
  TBX,
  i_JclNotify,
  i_ActiveMapsConfig;

type
  TActiveMapTBXItem = class(TTBXCustomItem)
  private
    FMapActive: IActiveMapSingle;
    FListener: IJclListener;
    procedure OnMapChangeState;
    procedure AdjustFont(Item: TTBCustomItem;
      Viewer: TTBItemViewer; Font: TFont; StateFlags: Integer);
  public
    constructor Create(AOwner: TComponent; AMapActive: IActiveMapSingle); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  u_NotifyEventListener;

{ TMiniMapTBXITem }

constructor TActiveMapTBXItem.Create(AOwner: TComponent;
  AMapActive: IActiveMapSingle);
begin
  inherited Create(AOwner);
  FMapActive := AMapActive;
  OnAdjustFont := Self.AdjustFont;
  FListener := TNotifyNoMmgEventListener.Create(Self.OnMapChangeState);
  FMapActive.GetChangeNotifier.Add(FListener);
  OnMapChangeState;
end;

destructor TActiveMapTBXItem.Destroy;
begin
  FMapActive.GetChangeNotifier.Remove(FListener);
  FListener := nil;
  inherited;
end;

procedure TActiveMapTBXItem.AdjustFont(Item: TTBCustomItem; Viewer: TTBItemViewer;
  Font: TFont; StateFlags: Integer);
begin
  if Self.Checked then begin
    Self.FontSettings.Bold := tsTrue;
  end else begin
    Self.FontSettings.Bold := tsDefault;
  end;
end;

procedure TActiveMapTBXItem.OnMapChangeState;
begin
  Self.Checked := FMapActive.GetIsActive;
end;

end.
