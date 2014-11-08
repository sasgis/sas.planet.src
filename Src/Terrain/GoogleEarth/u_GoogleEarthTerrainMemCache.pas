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

unit u_GoogleEarthTerrainMemCache;

interface

uses
  Types,
  Classes,
  SysUtils,
  libge;

type
  TGoogleEarthTerrainMemCache = class(TObject)
  private
    FList: TList;
    FCapacity: Integer;
    FCS: IReadWriteSync;
  public
    procedure Add(
      const AXY: TPoint;
      const AZoom: Byte;
      const AProvider: IGoogleEarthTerrainTileProvider
    );
    function Get(
      const AXY: TPoint;
      const AZoom: Byte;
      out AIsTne: Boolean
    ): IGoogleEarthTerrainTileProvider;
    procedure Clear;
  public
    constructor Create(const ACapacity: Integer = 500);
    destructor Destroy; override;
  end;

implementation

uses
  Windows,
  u_Synchronizer;

type
  TTerrainTileInfo = record
    X: Integer;
    Y: Integer;
    Zoom: Byte;
    IsTne: Boolean;
    LastAccess: Cardinal;
    Provider: IGoogleEarthTerrainTileProvider;
  end;
  PTerrainTileInfo = ^TTerrainTileInfo;

{ TGoogleEarthTerrainMemCache }

constructor TGoogleEarthTerrainMemCache.Create(const ACapacity: Integer);
begin
  inherited Create;
  FList := TList.Create;
  FCS := GSync.SyncVariable.Make(Self.ClassName);
  FCapacity := ACapacity;
end;

destructor TGoogleEarthTerrainMemCache.Destroy;
begin
  if Assigned(FCS) then begin
    Self.Clear;
  end;
  FreeAndNil(FList);
  FCS := nil;
  inherited;
end;

procedure TGoogleEarthTerrainMemCache.Add(
  const AXY: TPoint;
  const AZoom: Byte;
  const AProvider: IGoogleEarthTerrainTileProvider
);
var
  I: Integer;
  VTile: PTerrainTileInfo;
  VReplaceOld: Boolean;
  VOldestItem: Integer;
  VOldestAccessTime: Cardinal;
begin
  FCS.BeginWrite;
  try
    VOldestItem := -1;
    VOldestAccessTime := $FFFFFFFF;
    VReplaceOld := (FCapacity < FList.Count);

    for I := 0 to FList.Count - 1 do begin
      VTile := PTerrainTileInfo(FList.Items[I]);
      Assert(VTile <> nil);
      if (VTile.X = AXY.X) and
         (VTile.Y = AXY.Y) and
         (VTile.Zoom = AZoom)
      then begin
        VTile.IsTne := not Assigned(AProvider);
        VTile.Provider := AProvider;
        VTile.LastAccess := GetTickCount;
        Exit;
      end else begin
        if VTile.LastAccess < VOldestAccessTime then begin
          VOldestItem := I;
          VOldestAccessTime := VTile.LastAccess;
        end;
      end;
    end;

    if VReplaceOld then begin
      if (FList.Count > 0) and (FList.Count > VOldestItem) then begin
        VTile := PTerrainTileInfo(FList.Items[VOldestItem]);
        VTile.Provider := nil;
        Dispose(VTile);
        FList.Delete(VOldestItem);
      end;
    end;

    New(VTile);

    VTile.X := AXY.X;
    VTile.Y := AXY.Y;
    VTile.Zoom := AZoom;
    VTile.IsTne := not Assigned(AProvider);
    VTile.LastAccess := GetTickCount;
    VTile.Provider := AProvider;

    FList.Add(VTile);
  finally
    FCS.EndWrite;
  end;
end;

function TGoogleEarthTerrainMemCache.Get(
  const AXY: TPoint;
  const AZoom: Byte;
  out AIsTne: Boolean
): IGoogleEarthTerrainTileProvider;
var
  I: Integer;
  VTile: PTerrainTileInfo;
begin
  FCS.BeginWrite;
  try
    Result := nil;
    AIsTne := False;
    for I := 0 to FList.Count - 1 do begin
      VTile := PTerrainTileInfo(FList.Items[I]);
      if (VTile.X = AXY.X) and
         (VTile.Y = AXY.Y) and
         (VTile.Zoom = AZoom)
      then begin
        VTile.LastAccess := GetTickCount;
        AIsTne := VTile.IsTne;
        Result := VTile.Provider;
        Break;
      end;
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TGoogleEarthTerrainMemCache.Clear;
var
  I: Integer;
  VTile: PTerrainTileInfo;
begin
  FCS.BeginWrite;
  try
    for I := 0 to FList.Count - 1 do begin
      VTile := PTerrainTileInfo(FList.Items[I]);
      VTile.Provider := nil;
      Dispose(VTile);
    end;
    FList.Clear;
  finally
    FCS.EndWrite;
  end;
end;

end.
