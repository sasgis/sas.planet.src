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

unit u_TerrainProviderByGEMemCache deprecated;

interface

uses
  Types,
  Classes,
  SysUtils,
  i_GoogleEarthTerrain;

type
  PTerrainTile = ^TTerrainTile;
  TTerrainTile = record
    X: Integer;
    Y: Integer;
    Zoom: Byte;
    Exists: Boolean;
    LastAccess: Cardinal;
    Parser: IGoogleEarthTerrain;
  end;

  TTerrainProviderByGEMemCache = class(TObject)
  private
    FList: TList;
    FCapacity: Integer;
    FCS: IReadWriteSync;
  public
    constructor Create(const ACapacity: Integer);
    destructor Destroy; override;
    procedure Add(
      const AXY: TPoint;
      const AZoom: Byte;
      const AParser: IGoogleEarthTerrain
    );
    procedure AddTne(
      const AXY: TPoint;
      const AZoom: Byte
    );
    function Get(
      const AXY: TPoint;
      const AZoom: Byte
    ): PTerrainTile;
    procedure Clear;
  end;

implementation

uses
  Windows,
  u_Synchronizer;

constructor TTerrainProviderByGEMemCache.Create(const ACapacity: Integer);
begin
  inherited Create;
  FList := TList.Create;
  FCS := MakeSyncRW_Var(Self, False);
  FCapacity := ACapacity;
end;

destructor TTerrainProviderByGEMemCache.Destroy;
begin
  if Assigned(FCS) then begin
    Self.Clear;
  end;
  FreeAndNil(FList);
  inherited;
end;

procedure TTerrainProviderByGEMemCache.Add(
  const AXY: TPoint;
  const AZoom: Byte;
  const AParser: IGoogleEarthTerrain
);
var
  I: Integer;
  VTile: PTerrainTile;
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
      VTile := PTerrainTile(FList.Items[I]);
      if (VTile.X = AXY.X) and
         (VTile.Y = AXY.Y) and
         (VTile.Zoom = AZoom)
      then begin
        if VTile.Exists then begin
          raise Exception.CreateFmt(
            'Tile alredy in mem! X=%d Y=%d Z=%d', [AXY.X, AXY.Y, AZoom]
          );
        end else if Assigned(AParser) then begin
          VTile.Parser := AParser;
          VTile.LastAccess := GetTickCount;
          Exit;
        end else begin
          Exit;
        end;
      end else begin
        if VTile.LastAccess < VOldestAccessTime then begin
          VOldestItem := I;
          VOldestAccessTime := VTile.LastAccess;
        end;
      end;
    end;

    if VReplaceOld then begin
      if (FList.Count > 0) and (FList.Count > VOldestItem) then begin
        VTile := PTerrainTile(FList.Items[VOldestItem]);
        VTile.Parser := nil;
        Dispose(VTile);
        FList.Delete(VOldestItem);
      end;
    end;

    New(VTile);

    VTile.X := AXY.X;
    VTile.Y := AXY.Y;
    VTile.Zoom := AZoom;
    VTile.Exists := Assigned(AParser);
    VTile.LastAccess := GetTickCount;
    VTile.Parser := AParser;

    FList.Add(VTile);
  finally
    FCS.EndWrite;
  end;
end;

procedure TTerrainProviderByGEMemCache.AddTne(
  const AXY: TPoint;
  const AZoom: Byte
);
begin
  Self.Add(AXY, AZoom, nil);
end;

function TTerrainProviderByGEMemCache.Get(
  const AXY: TPoint;
  const AZoom: Byte
): PTerrainTile;
var
  I: Integer;
  VTile: PTerrainTile;
begin
  FCS.BeginWrite;
  try
    Result := nil;
    for I := 0 to FList.Count - 1 do begin
      VTile := PTerrainTile(FList.Items[I]);
      if (VTile.X = AXY.X) and
         (VTile.Y = AXY.Y) and
         (VTile.Zoom = AZoom)
      then begin
        VTile.LastAccess := GetTickCount;
        Result := VTile;
        Break;
      end;
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TTerrainProviderByGEMemCache.Clear;
var
  I: Integer;
  VTile: PTerrainTile;
begin
  FCS.BeginWrite;
  try
    for I := 0 to FList.Count - 1 do begin
      VTile := PTerrainTile(FList.Items[I]);
      VTile.Parser := nil;
      Dispose(VTile);
    end;
    FList.Clear;
  finally
    FCS.EndWrite;
  end;
end;

end.
