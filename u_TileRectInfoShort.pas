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

unit u_TileRectInfoShort;

interface

uses
  Types,
  i_TileIterator,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_TileInfoBasic,
  u_BaseInterfacedObject;

type
  TTileInfoShortInternal = record
    FLoadDate: TDateTime;
    FSize: Cardinal;
    FInfoType: TTileInfoType;
  end;
  TArrayOfTileInfoShortInternal = array of TTileInfoShortInternal;

  TTileRectInfoShort = class(TBaseInterfacedObject, ITileRectInfo)
  private
    FTileRect: TRect;
    FTileCount: TPoint;
    FZoom: Byte;
    FVersionInfo: IMapVersionInfo;
    FContentType: IContentTypeInfoBasic;
    FItems: TArrayOfTileInfoShortInternal;
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
      const AVersionInfo: IMapVersionInfo;
      const AContentType: IContentTypeInfoBasic;
      const AItems: TArrayOfTileInfoShortInternal
    );
    destructor Destroy; override;
  end;

implementation

{ TEnumTileInfoShort }

type
  PTileInfoShortInternalArray = ^TTileInfoShortInternalArray;
  TTileInfoShortInternalArray = array [0..0] of TTileInfoShortInternal;

  TEnumTileInfoShort = class(TBaseInterfacedObject, IEnumTileInfo)
  private
    FRef: IInterface;
    FVersionInfo: IMapVersionInfo;
    FContentType: IContentTypeInfoBasic;
    FZoom: Byte;
    FTileRect: TRect;
    FItems: PTileInfoShortInternalArray;
    FTileIterator: ITileIterator;
  private
    function Next(var ATileInfo: TTileInfo): Boolean;
  public
    constructor Create(
      const ARef: IInterface;
      const AVersionInfo: IMapVersionInfo;
      const AContentType: IContentTypeInfoBasic;
      const AZoom: Byte;
      const ATileRect: TRect;
      AItems: PTileInfoShortInternalArray;
      const ATileIterator: ITileIterator
    );
  end;

constructor TEnumTileInfoShort.Create(
  const ARef: IInterface;
  const AVersionInfo: IMapVersionInfo;
  const AContentType: IContentTypeInfoBasic;
  const AZoom: Byte;
  const ATileRect: TRect;
  AItems: PTileInfoShortInternalArray;
  const ATileIterator: ITileIterator
);
begin
  inherited Create;
  FRef := ARef;
  FVersionInfo := AVersionInfo;
  FContentType := AContentType;
  FZoom := AZoom;
  FTileRect := ATileRect;
  FItems := AItems;
  FTileIterator := ATileIterator;
end;

function TEnumTileInfoShort.Next(var ATileInfo: TTileInfo): Boolean;
var
  VTile: TPoint;
  VIndex: Integer;
begin
  Result := FTileIterator.Next(VTile);
  if Result then begin
    ATileInfo.FTile := VTile;
    ATileInfo.FZoom := FZoom;
    VIndex := TTileRectInfoShort.TileInRectToIndex(VTile, FTileRect);
    if VIndex < 0 then begin
      ATileInfo.FInfoType := titUnknown;
      ATileInfo.FContentType := nil;
      ATileInfo.FVersionInfo := nil;
      ATileInfo.FData := nil;
      ATileInfo.FSize := 0;
    end else begin
      ATileInfo.FInfoType := FItems[VIndex].FInfoType;
      ATileInfo.FContentType := FContentType;
      ATileInfo.FVersionInfo := FVersionInfo;
      ATileInfo.FData := nil;
      ATileInfo.FSize := FItems[VIndex].FSize;
      ATileInfo.FLoadDate := FItems[VIndex].FLoadDate;
    end;
  end;
end;

{ TTileRectInfoShort }

constructor TTileRectInfoShort.CreateWithOwn(
  const ATileRect: TRect;
  AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const AContentType: IContentTypeInfoBasic;
  const AItems: TArrayOfTileInfoShortInternal
);
begin
  Assert(AItems <> nil);
  Assert(Length(AItems) >= (ATileRect.Right - ATileRect.Left) * (FTileRect.Bottom - FTileRect.Top));
  inherited Create;
  FTileRect := ATileRect;
  FContentType := AContentType;
  FVersionInfo := AVersionInfo;
  FZoom := AZoom;
  FTileCount.X := FTileRect.Right - FTileRect.Left;
  FTileCount.Y := FTileRect.Bottom - FTileRect.Top;
  FItems := AItems;
end;

destructor TTileRectInfoShort.Destroy;
begin
  FItems := nil;
  inherited;
end;

function TTileRectInfoShort.GetEnum(
  const ATileIterator: ITileIterator
): IEnumTileInfo;
begin
  Result :=
    TEnumTileInfoShort.Create(
      Self,
      FVersionInfo,
      FContentType,
      FZoom,
      FTileRect,
      Addr(FItems[0]),
      ATileIterator
    );
end;

function TTileRectInfoShort.GetTileRect: TRect;
begin
  Result := FTileRect;
end;

function TTileRectInfoShort.GetZoom: Byte;
begin
  Result := FZoom;
end;

class function TTileRectInfoShort.TileInRectToIndex(const ATile: TPoint; const ARect: TRect): Integer;
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
