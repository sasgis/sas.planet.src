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

unit u_TileRectInfo;

interface

uses
  Types,
  i_BinaryData,
  i_TileIterator,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_TileInfoBasic,
  u_BaseInterfacedObject;

type
  TTileInfoInternal = record
    FLoadDate: TDateTime;
    FVersionInfo: IMapVersionInfo;
    FContentType: IContentTypeInfoBasic;
    FData: IBinaryData;
    FSize: Cardinal;
    FInfoType: TTileInfoType;
  end;
  TArrayOfTileInfoInternal = array of TTileInfoInternal;

  TTileRectInfo = class(TBaseInterfacedObject, ITileRectInfo)
  private
    FTileRect: TRect;
    FTileCount: TPoint;
    FZoom: Byte;
    FItems: TArrayOfTileInfoInternal;
  private
    function GetTileRect: TRect;
    function GetZoom: Byte;
    function GetEnum(const ATileIterator: ITileIterator): IEnumTileInfo;
  public
    class function TileInRectToIndex(const ATile: TPoint; const ARect: TRect): Integer;
  public
    constructor CreateWithOwn(
      const ATileRect: TRect;
      AZoom: Byte;
      const AItems: TArrayOfTileInfoInternal
    );
    destructor Destroy; override;
  end;

implementation

{ TEnumTileInfo }

type
  PTileInfoInternalArray = ^TTileInfoInternalArray;
  TTileInfoInternalArray = array [0..0] of TTileInfoInternal;

  TEnumTileInfo = class(TBaseInterfacedObject, IEnumTileInfo)
  private
    FRef: IInterface;
    FZoom: Byte;
    FTileRect: TRect;
    FItems: PTileInfoInternalArray;
    FTileIterator: ITileIterator;
  private
    function Next(var ATileInfo: TTileInfo): Boolean;
  public
    constructor Create(
      const ARef: IInterface;
      const AZoom: Byte;
      const ATileRect: TRect;
      AItems: PTileInfoInternalArray;
      const ATileIterator: ITileIterator
    );
  end;

constructor TEnumTileInfo.Create(
  const ARef: IInterface;
  const AZoom: Byte;
  const ATileRect: TRect;
  AItems: PTileInfoInternalArray;
  const ATileIterator: ITileIterator
);
begin
  inherited Create;
  FRef := ARef;
  FZoom := AZoom;
  FTileRect := ATileRect;
  FItems := AItems;
  FTileIterator := ATileIterator;
end;

function TEnumTileInfo.Next(var ATileInfo: TTileInfo): Boolean;
var
  VTile: TPoint;
  VIndex: Integer;
begin
  Result := FTileIterator.Next(VTile);
  if Result then begin
    ATileInfo.FTile := VTile;
    ATileInfo.FZoom := FZoom;
    VIndex := TTileRectInfo.TileInRectToIndex(VTile, FTileRect);
    if VIndex < 0 then begin
      ATileInfo.FInfoType := titUnknown;
      ATileInfo.FContentType := nil;
      ATileInfo.FVersionInfo := nil;
      ATileInfo.FData := nil;
      ATileInfo.FSize := 0;
    end else begin
      ATileInfo.FInfoType := FItems[VIndex].FInfoType;
      ATileInfo.FContentType := FItems[VIndex].FContentType;
      ATileInfo.FVersionInfo := FItems[VIndex].FVersionInfo;
      ATileInfo.FData := FItems[VIndex].FData;
      ATileInfo.FSize := FItems[VIndex].FSize;
      ATileInfo.FLoadDate := FItems[VIndex].FLoadDate;
    end;
  end;
end;

{ TTileRectInfo }

constructor TTileRectInfo.CreateWithOwn(
  const ATileRect: TRect;
  AZoom: Byte;
  const AItems: TArrayOfTileInfoInternal
);
begin
  Assert(AItems <> nil);
  Assert(Length(AItems) >= (ATileRect.Right - ATileRect.Left) * (FTileRect.Bottom - FTileRect.Top));
  inherited Create;
  FTileRect := ATileRect;
  FZoom := AZoom;
  FTileCount.X := FTileRect.Right - FTileRect.Left;
  FTileCount.Y := FTileRect.Bottom - FTileRect.Top;
  FItems := AItems;
end;

destructor TTileRectInfo.Destroy;
var
  VCount: Integer;
  i: Integer;
begin
  if FItems <> nil then begin
    VCount := FTileCount.X * FTileCount.Y;
    for i := 0 to VCount - 1 do begin
      FItems[i].FVersionInfo := nil;
      FItems[i].FContentType := nil;
      FItems[i].FData := nil;
    end;
    FItems := nil;
  end;
  inherited;
end;

function TTileRectInfo.GetEnum(const ATileIterator: ITileIterator): IEnumTileInfo;
begin
  Result := TEnumTileInfo.Create(Self, FZoom, FTileRect, Addr(FItems[0]), ATileIterator);
end;

function TTileRectInfo.GetTileRect: TRect;
begin
  Result := FTileRect;
end;

function TTileRectInfo.GetZoom: Byte;
begin
  Result := FZoom;
end;

class function TTileRectInfo.TileInRectToIndex(const ATile: TPoint;
  const ARect: TRect): Integer;
begin
  if (ATile.X < ARect.Left) or (ATile.X >= ARect.Right) then begin
    Result := -1;
  end else if (ATile.Y < ARect.Top) or (ATile.Y >= ARect.Bottom) then begin
    Result := -1;
  end else begin
    Result :=
      (ATile.X - ARect.Left) +
      (ATile.Y - ARect.Top) * (ARect.Right - ARect.Left);
  end;
end;

end.
