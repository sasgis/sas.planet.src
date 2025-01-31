{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_WikimapiaPlainTextParser;

interface

uses
  SysUtils,
  SynCommons,
  t_GeoTypes,
  i_BinaryData,
  i_VectorDataLoader,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_VectorItemSubset,
  i_VectorDataFactory,
  i_VectorDataItemSimple,
  i_VectorItemSubsetBuilder,
  u_BaseInterfacedObject;

type
  TWikiLineRec = record
    Count: Integer;
    Field: array[0..7] of record
      Ptr: PUTF8Char;
      Len: Integer;
    end;
  end;

  TWikimapiaPlainTextParser = class(TBaseInterfacedObject, IVectorDataLoader)
  private
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FVectorDataFactory: IVectorDataFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;

    class function ScanLine(
      var AData: PUTF8Char;
      const AEnd: PUTF8Char;
      out AInfo: TWikiLineRec
    ): Boolean; static;

    function ParseItem(
      const AContext: TVectorLoadContext;
      const AInfo: TWikiLineRec
    ): IVectorDataItem;

    function ParseTitle(const AInfo: TWikiLineRec): RawUTF8;
    function ParsePolygon(const AInfo: TWikiLineRec): IGeometryLonLat;
  private
    { IVectorDataLoader }
    function Load(
      const AContext: TVectorLoadContext;
      const AData: IBinaryData
    ): IVectorItemSubset;
  public
    constructor Create(
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
    );
  end;

implementation

{

Wikimapia tile is a text file divided into lines (with "\n" delimiter).

First line contains meta information about the tile, second line is empty,
every other line contains data about a place. Symbol "|" used as field separator.

1. First line format:

tileId|0|7

  - tileId — a unique tile identifier consisting of digits 0-3.
  - 0 — ?? indicates if the tile has children (sub tiles in the higher zoom level).
  - 7 — ?? bounds multiplier (1e7).

2. Place data format:

placeId|type|bounds|zoom|categories|titles|encodingType|polygon

  - placeId: unique place identifier (number);
  - type: place type (number);
  - bounds: place boundaries in the format "minLng,maxLng,minLat,maxLat" (encoded);
  - zoom: zoom level at which the place is visible (number);
  - categories: place categories, separated by commas;
  - titles: place names in different languages, separated by the "\x1f" (unit separator) character. Each name starts with a language code (encoded);
  - encodingType: polygon encoding type. The value 1 corresponds to the standard encoding format;
  - polygon: encoded polygon describing the shape of the place.

}

uses
  Types,
  i_DoublePointsAggregator,
  u_GeoFunc,
  u_DoublePointsAggregator;

{ TWikimapiaPlainTextParser }

constructor TWikimapiaPlainTextParser.Create(
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
);
begin
  inherited Create;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FVectorDataFactory := AVectorDataFactory;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
end;

function TWikimapiaPlainTextParser.Load(
  const AContext: TVectorLoadContext;
  const AData: IBinaryData
): IVectorItemSubset;
var
  VPtr: PUTF8Char;
  VEnd: PUTF8Char;
  VInfo: TWikiLineRec;
  VItem: IVectorDataItem;
  VSubsetBuilder: IVectorItemSubsetBuilder;
begin
  Result := nil;

  if (AData = nil) or (AData.Buffer = nil) or (AData.Size <= 0) then begin
    Exit;
  end;

  VSubsetBuilder := FVectorItemSubsetBuilderFactory.Build;

  VPtr := AData.Buffer;
  VEnd := VPtr + AData.Size;

  while ScanLine(VPtr, VEnd, VInfo) do begin
    VItem := ParseItem(AContext, VInfo);
    if VItem <> nil then begin
      VSubsetBuilder.Add(VItem);
    end;
  end;

  if VSubsetBuilder.Count > 0 then begin
    Result := VSubsetBuilder.MakeStaticAndClear;
  end;
end;

class function TWikimapiaPlainTextParser.ScanLine(
  var AData: PUTF8Char;
  const AEnd: PUTF8Char;
  out AInfo: TWikiLineRec
): Boolean;

  function _TryAddField(var P: PUTF8Char): Boolean;
  begin
    Result := AInfo.Count < Length(AInfo.Field);
    if not Result then begin
      Exit;
    end;

    with AInfo.Field[AInfo.Count] do begin
      Ptr := AData;
      Len := P - AData;
    end;

    Inc(AInfo.Count);
    Inc(P);
    AData := P;
  end;

var
  I: Integer;
  P: PUTF8Char;
begin
  Result := False;
  AInfo.Count := 0;

  P := AData;
  I := Length(AInfo.Field) - 1; // to detect if processing last field

  while P < AEnd do begin
    if P^ = #10 then begin
      if P <> AData then begin
        Result := _TryAddField(P);
      end else begin
        Inc(P);
        AData := P;
        Result := True;
      end;
      Exit;
    end else
    if (I > 0) and (P^ = '|') then begin
      Dec(I);
      Result := _TryAddField(P);
      if not Result then begin
        // error: too much fields!
        Exit;
      end;
    end else begin
      Inc(P);
    end;
  end;

  if (P = AEnd) and (AInfo.Count > 0) then begin
    // handle case if file doesn't finish with #10
    Result := _TryAddField(P);
    AData := AEnd;
  end;
end;

function TWikimapiaPlainTextParser.ParseItem(
  const AContext: TVectorLoadContext;
  const AInfo: TWikiLineRec
): IVectorDataItem;
var
  VDesc: string;
  VName, VPlaceId: RawUTF8;
  VPoly: IGeometryLonLat;
begin
  Result := nil;

  if AInfo.Count <> Length(AInfo.Field) then begin
    // skip header and empty lines
    Exit;
  end;

  // Place ID
  with AInfo.Field[0] do begin
    FastSetString(VPlaceId, Ptr, Len);
  end;

  VDesc := Format(
    '<a href=''http://wikimapia.org/%s/''>View or update this place information at Wikimapia.</a>',
    [UTF8ToString(VPlaceId)]
  );

  // Title
  VName := ParseTitle(AInfo);

  // Polygon
  VPoly := ParsePolygon(AInfo);
  if VPoly = nil then begin
    // polygon parsing error
    Exit;
  end;

  Result :=
    FVectorDataFactory.BuildItem(
      AContext.MainInfoFactory.BuildMainInfo(AContext.IdData, UTF8ToString(VName), VDesc),
      nil,
      VPoly
    );
end;

function TWikimapiaPlainTextParser.ParseTitle(const AInfo: TWikiLineRec): RawUTF8;
var
  VTitle: array of record
    Ptr: PUTF8Char;
    Len: Integer;
    Lng: Integer;  // 0 - english, 1 - russian, etc
  end;

  procedure _AddTitle(var AStartPtr, ACurrPtr: PUTF8Char);
  var
    I: Integer;
  begin
    I := Length(VTitle);
    SetLength(VTitle, I+1);

    with VTitle[I] do begin
      Ptr := AStartPtr + 1;
      Len := ACurrPtr - AStartPtr - 1;
      Lng := PByte(AStartPtr)^ - 32;
    end;

    Inc(ACurrPtr);
    AStartPtr := ACurrPtr;
  end;

  function _GetTitleIndexByLangId(const ALangId: Integer): Integer;
  var
    I: Integer;
    VId: Integer;
    VEngIndex: Integer;
  begin
    if Length(VTitle) = 1 then begin
      Result := 0;
      Exit;
    end;

    VEngIndex := -1;

    for I := 0 to Length(VTitle) - 1 do begin
      VId := VTitle[I].Lng;
      if VId = ALangId then begin
        Result := I; // found requested lang
        Exit;
      end;
      if VId = 0 then begin
        VEngIndex := I;
      end;
    end;

    if VEngIndex >= 0 then begin
      // requested lang is not found but English is available, so use it
      Result := VEngIndex;
    end else begin
      Result := 0; // return first available lang
    end;
  end;

var
  I: Integer;
  VPtr, VCurr, VEnd: PUTF8Char;
begin
  VTitle := nil;

  VPtr := AInfo.Field[5].Ptr;
  VEnd := VPtr + AInfo.Field[5].Len;

  VCurr := VPtr;
  while VCurr <= VEnd do begin
    if VCurr = VEnd then begin
      _AddTitle(VPtr, VCurr);
      Break;
    end else
    if PByte(VCurr)^ = $1F then begin
      _AddTitle(VPtr, VCurr);
    end else begin
      Inc(VCurr);
    end;
  end;

  if Length(VTitle) = 0 then begin
    Result := '';
  end else begin
    I := _GetTitleIndexByLangId(1); // todo: use user lang id
    with VTitle[I] do begin
      FastSetString(Result, Ptr, Len);
    end;
  end;
end;

function TWikimapiaPlainTextParser.ParsePolygon(const AInfo: TWikiLineRec): IGeometryLonLat;

  function _DecodeCoord(var APtr: PByte; const AEnd: PByte; var ACoord: Integer): Boolean;
  var
    p, l, c: Integer;
  begin
    l := 0;
    c := 0;
    repeat
      if APtr >= AEnd then begin
        Result := False;
        Exit;
      end;
      p := APtr^ - 63;
      c := c or ((p and 31) shl l);
      Inc(l, 5);
      Inc(APtr);
    until p < 32;

    if c and 1 <> 0 then begin
      Inc(ACoord, not (c shr 1));
    end else begin
      Inc(ACoord, c shr 1);
    end;

    Result := True;
  end;

var
  VPtr, VEnd: PByte;
  VLon, VLat: Integer;
  VPoints: IDoublePointsAggregator;
  VBuilder: IGeometryLonLatPolygonBuilder;
begin
  Result := nil;

  with AInfo.Field[6] do begin
    if (Len <> 1) or (Ptr^ <> '1') then begin
      Assert(False, 'Wikimapia: Unknown polygon encoding algorithm!');
      Exit;
    end;
  end;

  VPtr := PByte(AInfo.Field[7].Ptr);
  VEnd := VPtr + AInfo.Field[7].Len;

  VLon := 0;
  VLat := 0;

  VPoints := TDoublePointsAggregator.Create;

  while VPtr < VEnd do begin
    if not _DecodeCoord(VPtr, VEnd, VLon) or
       not _DecodeCoord(VPtr, VEnd, VLat)
    then begin
      Assert(False);
      Exit;
    end;

    VPoints.Add( DoublePoint(VLon / 1e6, VLat / 1e6) );
  end;

  if VPoints.Count >= 3 then begin
    VBuilder := FVectorGeometryLonLatFactory.MakePolygonBuilder;
    VBuilder.AddOuter(VPoints.MakeStaticAndClear);

    Result := VBuilder.MakeStaticAndClear;
  end;
end;

end.
