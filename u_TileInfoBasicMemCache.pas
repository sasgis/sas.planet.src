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

unit u_TileInfoBasicMemCache;

interface

uses
  Windows,
  Classes,
  SysUtils,
  i_MapVersionInfo,
  i_TileInfoBasic;

type
  TTileInfoBasicMemCache = class
  private
    FList: TList;
    FMaxTileInfoCounts: Integer;
    FTileInfoTTL: Cardinal;
    FCS: IReadWriteSync;
  public
    constructor Create(
      const AMaxTileInfoCounts: Integer;
      const ATileInfoTTL: Cardinal
    );
    destructor Destroy; override;

    procedure Add(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ATileInfoBasic: ITileInfoBasic
    );

    procedure Remove(
      const AXY: TPoint;
      const AZoom: Byte
    );

    function Get(
      const AXY: TPoint;
      const AZoom: Byte;
      const AUpdateTTL: Boolean = True
    ): ITileInfoBasic;

    procedure Clear;

    procedure ClearByTTL;
  end;

implementation

uses
  u_Synchronizer;

type
  TTileInfoCacheRec = record
    TileTTL: Cardinal;
    TileXY: TPoint;
    TileZoom: Byte;
    TileVersionInfo: IMapVersionInfo;
    TileInfoBasic: ITileInfoBasic;
  end;
  PTileInfoCacheRec = ^TTileInfoCacheRec;

{ TTileInfoBasicMemCache }

constructor TTileInfoBasicMemCache.Create(
  const AMaxTileInfoCounts: Integer;
  const ATileInfoTTL: Cardinal
);
begin
  inherited Create;
  FCS := MakeSyncRW_Var(Self, False);
  FList := TList.Create;
  FMaxTileInfoCounts := AMaxTileInfoCounts;
  FTileInfoTTL := ATileInfoTTL;
end;

destructor TTileInfoBasicMemCache.Destroy;
begin
  Self.Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TTileInfoBasicMemCache.Add(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const ATileInfoBasic: ITileInfoBasic
);
var
  I: Integer;
  VTile: PTileInfoCacheRec;
  VReplaceOld: Boolean;
  VOldestItem: Integer;
  VMinTTL: Cardinal;
begin
  FCS.BeginWrite;
  try
    VOldestItem := -1;
    VMinTTL := $FFFFFFFF;
    VReplaceOld := (FMaxTileInfoCounts < FList.Count);

    for I := 0 to FList.Count - 1 do begin
      VTile := PTileInfoCacheRec(FList.Items[I]);
      if (VTile.TileXY.X = AXY.X) and
         (VTile.TileXY.Y = AXY.Y) and
         (VTile.TileZoom = AZoom)
      then begin
        VTile.TileTTL := GetTickCount + FTileInfoTTL;
        VTile.TileVersionInfo := AVersionInfo;
        VTile.TileInfoBasic := ATileInfoBasic;
        Exit;
      end else begin
        if VTile.TileTTL < VMinTTL then begin
          VOldestItem := I;
          VMinTTL := VTile.TileTTL;
        end;
      end;
    end;

    if VReplaceOld then begin
      if (FList.Count > 0) and (FList.Count > VOldestItem) then begin
        VTile := PTileInfoCacheRec(FList.Items[VOldestItem]);
        VTile.TileVersionInfo := nil;
        VTile.TileInfoBasic := nil;
        Dispose(VTile);
        FList.Delete(VOldestItem);
      end;
    end;

    New(VTile);

    VTile.TileTTL := GetTickCount + FTileInfoTTL;
    VTile.TileXY := AXY;
    VTile.TileZoom := AZoom;
    VTile.TileVersionInfo := AVersionInfo;
    VTile.TileInfoBasic := ATileInfoBasic;

    FList.Add(VTile);
  finally
    FCS.EndWrite;
  end;
end;

procedure TTileInfoBasicMemCache.Remove(
  const AXY: TPoint;
  const AZoom: Byte
);
var
  I: Integer;
  VTile: PTileInfoCacheRec;
begin
  FCS.BeginWrite;
  try
    for I := 0 to FList.Count - 1 do begin
      VTile := PTileInfoCacheRec(FList.Items[I]);
      if (VTile.TileXY.X = AXY.X) and
         (VTile.TileXY.Y = AXY.Y) and
         (VTile.TileZoom = AZoom)
      then begin
        VTile := PTileInfoCacheRec(FList.Items[I]);
        VTile.TileVersionInfo := nil;
        VTile.TileInfoBasic := nil;
        Dispose(VTile);
        FList.Delete(I);
        Break;
      end;
    end;
  finally
    FCS.EndWrite;
  end;
end;

function TTileInfoBasicMemCache.Get(
  const AXY: TPoint;
  const AZoom: Byte;
  const AUpdateTTL: Boolean = True
): ITileInfoBasic;
var
  I: Integer;
  VTile: PTileInfoCacheRec;
begin
  FCS.BeginWrite;
  try
    Result := nil;
    for I := 0 to FList.Count - 1 do begin
      VTile := PTileInfoCacheRec(FList.Items[I]);
      if (VTile.TileXY.X = AXY.X) and
         (VTile.TileXY.Y = AXY.Y) and
         (VTile.TileZoom = AZoom)
      then begin
        if (VTile.TileTTL < GetTickCount) then begin
          VTile := PTileInfoCacheRec(FList.Items[I]);
          VTile.TileVersionInfo := nil;
          VTile.TileInfoBasic := nil;
          Dispose(VTile);
          FList.Delete(I);
        end else begin
          if AUpdateTTL then begin
            VTile.TileTTL := GetTickCount + FTileInfoTTL;
          end;
          Result := VTile.TileInfoBasic;
        end;
        Break;
      end;
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TTileInfoBasicMemCache.Clear;
var
  I: Integer;
  VTile: PTileInfoCacheRec;
begin
  FCS.BeginWrite;
  try
    for I := 0 to FList.Count - 1 do begin
      VTile := PTileInfoCacheRec(FList.Items[I]);
      VTile.TileVersionInfo := nil;
      VTile.TileInfoBasic := nil;
      Dispose(VTile);
    end;
    FList.Clear;
  finally
    FCS.EndWrite;
  end;
end;

procedure TTileInfoBasicMemCache.ClearByTTL;
var
  I: Integer;
  VTile: PTileInfoCacheRec;
begin
  FCS.BeginWrite;
  try
    I := 0;
    while I < FList.Count do begin
      VTile := PTileInfoCacheRec(FList.Items[I]);
      if (VTile.TileTTL < GetTickCount) then begin
        VTile := PTileInfoCacheRec(FList.Items[I]);
        VTile.TileVersionInfo := nil;
        VTile.TileInfoBasic := nil;
        Dispose(VTile);
        FList.Delete(I);
      end else begin
        Inc(I);
      end;
    end;
    FList.Pack;
  finally
    FCS.EndWrite;
  end;
end;

end.
