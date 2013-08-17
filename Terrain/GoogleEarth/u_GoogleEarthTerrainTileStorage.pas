{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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

unit u_GoogleEarthTerrainTileStorage;

interface

uses
  Windows,
  SysUtils,
  libge,
  i_Notifier,
  i_GoogleEarthTerrainTileStorage,
  u_GoogleEarthTerrainMemCache,
  u_BaseInterfacedObject;

type
  TGoogleEarthTerrainTileStorage = class(TBaseInterfacedObject, IGoogleEarthTerrainTileStorage)
  private
    FCachePath: string;
    FCacheProvider: IGoogleEarthCacheProvider;
    FSync: IReadWriteSync;
    FNotifier: INotifier;
    FNotifierInternal: INotifierInternal;
    FMemCache: TGoogleEarthTerrainMemCache;
  private
    function BuildCacheProvider(const APath: string): IGoogleEarthCacheProvider;
  private
    { IGoogleEarthTerrainTileStorage }
    function GetTileInfo(
      const AXY: TPoint;
      const AZoom: Byte
    ): IGoogleEarthTerrainTileProvider;
    function SetPath(const APath: string): Boolean;
    function GetAvailable: Boolean;
    function GetNotifier: INotifier;
  public
    constructor Create(
      const AStoragePath: string
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Notifier,
  u_Synchronizer;

{ TGoogleEarthTerrainTileStorage }

constructor TGoogleEarthTerrainTileStorage.Create(
  const AStoragePath: string
);
begin
  inherited Create;
  FSync := MakeSyncRW_Big(Self);

  FNotifierInternal := TNotifierBase.Create;
  FNotifier := FNotifierInternal;

  FMemCache := TGoogleEarthTerrainMemCache.Create;

  FSync.BeginWrite;
  try
    FCachePath := AStoragePath;
    FCacheProvider := BuildCacheProvider(AStoragePath);
  finally
    FSync.EndWrite;
  end;
end;

destructor TGoogleEarthTerrainTileStorage.Destroy;
begin
  FNotifier := nil;
  FNotifierInternal := nil;
  FCacheProvider := nil;
  FSync := nil;
  FMemCache.Free;
  inherited;
end;

function TGoogleEarthTerrainTileStorage.BuildCacheProvider(
  const APath: string
): IGoogleEarthCacheProvider;
var
  VCacheFactory: IGoogleEarthCacheProviderFactory;
begin
  VCacheFactory := CreateGoogleEarthCacheProviderFactory;
  if VCacheFactory <> nil then begin
    Result := VCacheFactory.CreateEarthTerrainProvider(PAnsiChar(APath));
  end else begin
    Result := nil;
  end;
end;

function TGoogleEarthTerrainTileStorage.GetTileInfo(
  const AXY: TPoint;
  const AZoom: Byte
): IGoogleEarthTerrainTileProvider;
var
  VIsTne: Boolean;
  VResult: Boolean;
  VData: IInterface;
  VOutTileVersion: Word;
  VOutTileDate: TDateTime;
  VTileSize: Integer;
begin
  Result := FMemCache.Get(AXY, AZoom, VIsTne);
  
  if (Result = nil) and not VIsTne then begin

    FSync.BeginRead;
    try
      if (FCacheProvider <> nil) then begin
        VResult := FCacheProvider.GetTileInfo(
          AXY,
          AZoom,
          0,
          0,
          True,
          True,
          True,
          VTileSize,
          VOutTileVersion,
          VOutTileDate,
          VData
        );

        if VResult then begin
          Supports(VData, IGoogleEarthTerrainTileProvider, Result);
        end;
      end;
    finally
      FSync.EndRead;
    end;

    FMemCache.Add(AXY, AZoom, Result);
  end;
end;

function TGoogleEarthTerrainTileStorage.SetPath(const APath: string): Boolean;
begin
  Result := False;

  FSync.BeginWrite;
  try
    if not SameText(FCachePath, APath) then begin
      FMemCache.Clear;
      FCachePath := APath;
      FCacheProvider := BuildCacheProvider(FCachePath);
      Result := True;
    end;
  finally
    FSync.EndWrite;
  end;

  if Result then begin
    FNotifierInternal.Notify(nil);
  end;
end;

function TGoogleEarthTerrainTileStorage.GetAvailable: Boolean;
begin
  FSync.BeginRead;
  try
    Result := (FCacheProvider <> nil);
  finally
    FSync.EndRead;
  end;
end;

function TGoogleEarthTerrainTileStorage.GetNotifier: INotifier;
begin
  Result := FNotifier;
end;

end.
