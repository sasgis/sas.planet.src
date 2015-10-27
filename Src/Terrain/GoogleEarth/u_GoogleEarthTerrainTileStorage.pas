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
    FAvailable: Boolean;
    FCachePath: AnsiString;
    FCacheProvider: IGoogleEarthCacheProvider;
    FSync: IReadWriteSync;
    FNotifier: INotifier;
    FNotifierInternal: INotifierInternal;
    FMemCache: TGoogleEarthTerrainMemCache;
  private
    function BuildCacheProvider: Boolean;
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
  FSync := GSync.SyncVariable.Make(Self.ClassName);

  FNotifierInternal :=
    TNotifierBase.Create(
      GSync.SyncVariable.Make(Self.ClassName + 'Notifier')
    );
  FNotifier := FNotifierInternal;

  FMemCache := TGoogleEarthTerrainMemCache.Create;

  FCachePath := AStoragePath;  // TODO: Fix for unicode path
  FCacheProvider := nil;
  FAvailable := True;
end;

destructor TGoogleEarthTerrainTileStorage.Destroy;
begin
  FNotifier := nil;
  FNotifierInternal := nil;
  FCacheProvider := nil;
  FSync := nil;
  FreeAndNil(FMemCache);
  inherited;
end;

function TGoogleEarthTerrainTileStorage.BuildCacheProvider: Boolean;
var
  VOpenErrorMsg: WideString;
  VCacheFactory: IGoogleEarthCacheProviderFactory;
begin
  FSync.BeginWrite;
  try
    VOpenErrorMsg := '';
    if FAvailable and (FCacheProvider = nil) then begin
      VCacheFactory := libge.CreateGoogleEarthCacheProviderFactory;
      if VCacheFactory <> nil then begin
        FCacheProvider := VCacheFactory.CreateEarthTerrainProvider(PAnsiChar(FCachePath), VOpenErrorMsg);
      end;
    end;
    FAvailable := (VOpenErrorMsg = '') and (FCacheProvider <> nil);
    Result := FAvailable;
  finally
    FSync.EndWrite;
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
  VAvailable: Boolean;
begin
  Result := FMemCache.Get(AXY, AZoom, VIsTne);

  if (Result = nil) and not VIsTne then begin

    FSync.BeginRead;
    try
      VAvailable := FAvailable and (FCacheProvider <> nil);
    finally
      FSync.EndRead;
    end;

    if not VAvailable then begin
      if BuildCacheProvider then begin
        FNotifierInternal.Notify(nil);
      end else begin
        Exit;
      end;
    end;

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
  FSync.BeginWrite;
  try
    FMemCache.Clear;
    FCachePath := APath; // TODO: Fix for unicode path
    FCacheProvider := nil;
    FAvailable := True;
    Result := True;
  finally
    FSync.EndWrite;
  end;
  FNotifierInternal.Notify(nil);
end;

function TGoogleEarthTerrainTileStorage.GetAvailable: Boolean;
begin
  FSync.BeginRead;
  try
    Result := FAvailable;
  finally
    FSync.EndRead;
  end;
end;

function TGoogleEarthTerrainTileStorage.GetNotifier: INotifier;
begin
  Result := FNotifier;
end;

end.
