{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_FavoriteMapSetHotKeyList;

interface

uses
  Classes,
  SysUtils,
  i_IDList,
  i_Listener,
  i_Notifier,
  i_FavoriteMapSetConfig,
  i_FavoriteMapSetItemStatic,
  i_FavoriteMapSetHotKeyList,
  u_BaseInterfacedObject;

type
  TFavoriteMapSetHotKeyList = class(TBaseInterfacedObject, IFavoriteMapSetHotKeyList)
  private
    FLock: IReadWriteSync;
    FList: IIDInterfaceList;
    FListener: IListener;
    FNotifier: INotifier;
    FFavoriteMapSetConfig: IFavoriteMapSetConfig;
    procedure OnMapSetChange;
  private
    { IFavoriteMapSetHotKeyList }
    function GetMapSetByHotKey(const AHotKey: TShortCut): IFavoriteMapSetItemStatic;
  public
    constructor Create(const AFavoriteMapSetConfig: IFavoriteMapSetConfig);
    destructor Destroy; override;
  end;

implementation

uses
  i_InterfaceListStatic,
  u_ListenerByEvent,
  u_IDInterfaceList,
  u_Synchronizer;

{ TFavoriteMapSetHotKeyList }

constructor TFavoriteMapSetHotKeyList.Create(
  const AFavoriteMapSetConfig: IFavoriteMapSetConfig
);
begin
  Assert(AFavoriteMapSetConfig <> nil);
  inherited Create;

  FFavoriteMapSetConfig := AFavoriteMapSetConfig;

  FList := TIDInterfaceList.Create;
  FLock := GSync.SyncStd.Make(Self.ClassName);

  FNotifier := FFavoriteMapSetConfig.ChangeNotifier;
  if FNotifier <> nil then begin
    FListener := TNotifyNoMmgEventListener.Create(Self.OnMapSetChange);
    FNotifier.Add(FListener);
  end;

  OnMapSetChange;
end;

procedure TFavoriteMapSetHotKeyList.OnMapSetChange;
var
  I: Integer;
  VItem: IFavoriteMapSetItemStatic;
  VStatic: IInterfaceListStatic;
begin
  FLock.BeginWrite;
  try
    FList.Clear;
    VStatic := FFavoriteMapSetConfig.GetStatic;
    if VStatic <> nil then begin
      for I := 0 to VStatic.Count - 1 do begin
        VItem := VStatic.Items[I] as IFavoriteMapSetItemStatic;
        if VItem.HotKey <> 0 then begin
          FList.Add(VItem.HotKey, VItem);
        end;
      end;
    end;
  finally
    FLock.EndWrite;
  end;
end;

destructor TFavoriteMapSetHotKeyList.Destroy;
begin
  if (FListener <> nil) and (FNotifier <> nil) then begin
    FNotifier.Remove(FListener);
    FNotifier := nil;
    FListener := nil;
  end;
  inherited;
end;

function TFavoriteMapSetHotKeyList.GetMapSetByHotKey(
  const AHotKey: TShortCut
): IFavoriteMapSetItemStatic;
begin
  FLock.BeginRead;
  try
    Result := IFavoriteMapSetItemStatic(FList.GetByID(AHotKey));
  finally
    FLock.EndRead;
  end;
end;

end.
