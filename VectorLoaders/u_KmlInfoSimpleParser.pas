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

unit u_KmlInfoSimpleParser;

interface

uses
  Classes,
  SysUtils,
  t_GeoTypes,
  i_BinaryData,
  i_VectorItemSubsetBuilder,
  i_VectorDataFactory,
  i_GeometryLonLatFactory,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_DoublePointsAggregator,
  i_VectorDataLoader,
  BMSEARCH,
  u_BaseInterfacedObject;

type
  TKmlInfoSimpleParser = class(TBaseInterfacedObject, IVectorDataLoader)
  private
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorDataFactory: IVectorDataFactory;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;

    FFormat: TFormatSettings;
    FBMSrchPlacemark: TSearchBM;
    FBMSrchPlacemarkE: TSearchBM;
    FBMSrchName: TSearchBM;
    FBMSrchCloseQ: TSearchBM;
    FBMSrchNameE: TSearchBM;
    FBMSrchId: TSearchBM;
    FBMSrchDesc: TSearchBM;
    FBMSrchDescE: TSearchBM;
    FBMSrchCoord: TSearchBM;
    FBMSrchCoordE: TSearchBM;
    function PosOfChar(
      APattern: AnsiChar;
      AText: PAnsiChar;
      ALast: PAnsiChar
    ): PAnsiChar;
    function PosOfNonSpaceChar(
      AText: PAnsiChar;
      ALast: PAnsiChar
    ): PAnsiChar;
    function PosOfSpaceChar(
      AText: PAnsiChar;
      ALast: PAnsiChar
    ): PAnsiChar;
    function parse(
      const buffer: AnsiString;
      const AList: IVectorItemSubsetBuilder;
      const AIdData: Pointer;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
    ): boolean;
    function parseCoordinates(
      AText: PAnsiChar;
      ALen: integer;
      const APointsAggregator: IDoublePointsAggregator
    ): boolean;
    function parseName(Name: AnsiString): string;
    function parseDescription(Description: AnsiString): string;
    function BuildItem(
      const AName, ADesc: string;
      const APointsAggregator: IDoublePointsAggregator;
      const AIdData: Pointer;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
    ): IVectorDataItemSimple;
    function LoadFromStreamInternal(
      AStream: TStream;
      const AIdData: Pointer;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
    ): IVectorItemSubset;
  private
    function Load(
      const AData: IBinaryData;
      const AIdData: Pointer;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
    ): IVectorItemSubset;
  public
    constructor Create(
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory
    );
    destructor Destroy; override;
  end;

implementation

uses
  ALString,
  cUnicodeCodecs,
  i_GeometryLonLat,
  u_StreamReadOnlyByBinaryData,
  u_DoublePointsAggregator,
  u_GeoFunc;

{ TKmlInfoSimpleParser }

function TKmlInfoSimpleParser.BuildItem(
  const AName, ADesc: string;
  const APointsAggregator: IDoublePointsAggregator;
  const AIdData: Pointer;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
): IVectorDataItemSimple;
var
  VPointCount: Integer;
  VPoint: IGeometryLonLatPoint;
  VPath: IGeometryLonLatMultiLine;
  VPoly: IGeometryLonLatMultiPolygon;
begin
  Result := nil;
  VPointCount := APointsAggregator.Count;
  if VPointCount > 0 then begin
    if VPointCount = 1 then begin
      if not PointIsEmpty(APointsAggregator.Points[0]) then begin
        VPoint := FVectorGeometryLonLatFactory.CreateLonLatPoint(APointsAggregator.Points[0]);
        Result :=
          FVectorDataFactory.BuildPoint(
            AVectorDataItemMainInfoFactory.BuildMainInfo(AIdData, AName, ADesc),
            nil,
            VPoint
          );
      end;
    end else begin
      if DoublePointsEqual(APointsAggregator.Points[0], APointsAggregator.Points[VPointCount - 1]) then begin
        VPoly := FVectorGeometryLonLatFactory.CreateLonLatMultiPolygon(APointsAggregator.Points, VPointCount);
        if Assigned(VPoly) then begin
          Result :=
            FVectorDataFactory.BuildPoly(
              AVectorDataItemMainInfoFactory.BuildMainInfo(AIdData, AName, ADesc),
              nil,
              VPoly
            );
        end;
      end else begin
        VPath := FVectorGeometryLonLatFactory.CreateLonLatMultiLine(APointsAggregator.Points, VPointCount);
        if Assigned(VPath) then begin
          Result :=
            FVectorDataFactory.BuildPath(
              AVectorDataItemMainInfoFactory.BuildMainInfo(AIdData, AName, ADesc),
              nil,
              VPath
            );
        end;
      end;
    end;
  end;
end;

constructor TKmlInfoSimpleParser.Create(
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory
);
begin
  inherited Create;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FVectorDataFactory := AVectorDataFactory;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;

  FFormat.DecimalSeparator := '.';
  FBMSrchPlacemark := TSearchBM.Create('<Placemark');
  FBMSrchPlacemarkE := TSearchBM.Create('</Placemark');
  FBMSrchName := TSearchBM.Create('<name');
  FBMSrchCloseQ := TSearchBM.Create('>');
  FBMSrchNameE := TSearchBM.Create('</name');
  FBMSrchId := TSearchBM.Create('id=');
  FBMSrchDesc := TSearchBM.Create('<description');
  FBMSrchDescE := TSearchBM.Create('</description');
  FBMSrchCoord := TSearchBM.Create('<coordinates');
  FBMSrchCoordE := TSearchBM.Create('</coordinates');
end;

destructor TKmlInfoSimpleParser.Destroy;
begin
  FreeAndNil(FBMSrchPlacemark);
  FreeAndNil(FBMSrchPlacemarkE);
  FreeAndNil(FBMSrchName);
  FreeAndNil(FBMSrchCloseQ);
  FreeAndNil(FBMSrchNameE);
  FreeAndNil(FBMSrchId);
  FreeAndNil(FBMSrchDesc);
  FreeAndNil(FBMSrchDescE);
  FreeAndNil(FBMSrchCoord);
  FreeAndNil(FBMSrchCoordE);
  inherited;
end;

function TKmlInfoSimpleParser.Load(
  const AData: IBinaryData;
  const AIdData: Pointer;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
): IVectorItemSubset;
var
  VStream: TStreamReadOnlyByBinaryData;
begin
  Result := nil;
  VStream := TStreamReadOnlyByBinaryData.Create(AData);
  try
    Result := LoadFromStreamInternal(VStream, AIdData, AVectorDataItemMainInfoFactory);
  finally
    VStream.Free;
  end;
end;

function TKmlInfoSimpleParser.LoadFromStreamInternal(AStream: TStream;
  const AIdData: Pointer;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
): IVectorItemSubset;
  function GetAnsiString(AStream: TStream): AnsiString;
  var
    VBOMSize: Integer;
    VKmlDoc: Pointer;
    VKmlDocSize: Integer;
    VUnicodeCodec: TUnicodeCodecClass;
    VCustomCodec: TCustomUnicodeCodec;
    VStr: WideString;
  begin
    VKmlDocSize := AStream.Size;
    GetMem(VKmlDoc, VKmlDocSize);
    try
      Result := '';
      AStream.Position := 0;
      AStream.ReadBuffer(VKmlDoc^, VKmlDocSize);
      VUnicodeCodec := DetectUTFEncoding(VKmlDoc, VKmlDocSize, VBOMSize);
      if VUnicodeCodec <> nil then begin
        VCustomCodec := VUnicodeCodec.Create;
        try
          VCustomCodec.DecodeStr(VKmlDoc, VKmlDocSize, VStr);
          Result := Utf8Encode(VStr); // парсер KML воспринимает только UTF-8
        finally
          VCustomCodec.Free;
        end;
      end else begin
        AStream.Position := 0;
        SetLength(Result, AStream.Size);
        AStream.ReadBuffer(Result[1], AStream.Size);
      end;
    finally
      FreeMem(VKmlDoc);
    end;
  end;
var
  VKml: AnsiString;
  VList: IVectorItemSubsetBuilder;
begin
  Result := nil;
  if AStream.Size > 0 then begin
    VKml := GetAnsiString(AStream);
    if VKml <> '' then begin
      VList := FVectorItemSubsetBuilderFactory.Build;
      parse(VKml, VList, AIdData, AVectorDataItemMainInfoFactory);
      Result := VList.MakeStaticAndClear;
    end else begin
      Assert(False, 'KML data reader - Unknown error');
    end;
  end;
end;

function TKmlInfoSimpleParser.parseName(Name: AnsiString): string;
var
  pb: integer;
begin
  pb := ALPosEx('<![CDATA[', Name, 1);
  if pb > 0 then begin
    Name := ALCopyStr(Name, pb + 9, ALPosEx(']]>', Name, 1) - (pb + 9));
  end;
  Result := Utf8ToAnsi(Name);
end;

function TKmlInfoSimpleParser.parseDescription(Description: AnsiString): string;
var
  pb: integer;
  iip: integer;
begin
  pb := ALPosEx('<![CDATA[', Description, 1);
  if pb > 0 then begin
    Description := ALCopyStr(Description, pb + 9, ALPosEx(']]>', Description, 1) - (pb + 9));
  end;
  iip := ALPosEx('&lt;', Description, 1);
  while iip > 0 do begin
    Description[iip] := '<';
    Delete(Description, iip + 1, 3);
    iip := ALPosEx('&lt;', Description, iip);
  end;
  iip := ALPosEx('&gt;', Description, 1);
  while iip > 0 do begin
    Description[iip] := '>';
    Delete(Description, iip + 1, 3);
    iip := ALPosEx('&gt;', Description, iip);
  end;
  Result := Utf8ToAnsi(Description);
end;

function TKmlInfoSimpleParser.parse(
  const buffer: AnsiString;
  const AList: IVectorItemSubsetBuilder;
  const AIdData: Pointer;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
): boolean;
var
  position, PosStartPlace, PosTag1, PosTag2, PosTag3, PosEndPlace, sLen: integer;
  sStart: Cardinal;
  VName: string;
  VDescription: string;
  VItem: IVectorDataItemSimple;
  VPointsAggregator: IDoublePointsAggregator;
begin
  result := true;
  sLen := Length(buffer);
  sStart := Cardinal(@buffer[1]);
  position := 1;
  PosStartPlace := 1;
  PosEndPlace := 1;
  VPointsAggregator := TDoublePointsAggregator.Create;
  While (position > 0) and (PosStartPlace > 0) and (PosEndPlace > 0) and (result) do begin
    try
      PosStartPlace := Cardinal(FBMSrchPlacemark.Search(@buffer[position], sLen - position + 1)) - sStart + 1;
      if PosStartPlace > 0 then begin
        PosEndPlace := Cardinal(FBMSrchPlacemarkE.Search(@buffer[PosStartPlace], sLen - PosStartPlace + 1)) - sStart + 1;
        if PosEndPlace > 0 then begin
          VName := '';
          position := Cardinal(FBMSrchId.Search(@buffer[PosStartPlace], PosEndPlace - PosStartPlace + 1)) - sStart + 1;
          PosTag1 := Cardinal(FBMSrchName.Search(@buffer[PosStartPlace], PosEndPlace - PosStartPlace + 1)) - sStart + 1;
          if (PosTag1 > PosStartPlace) and (PosTag1 < PosEndPlace) then begin
            PosTag2 := Cardinal(FBMSrchCloseQ.Search(@buffer[PosTag1], PosEndPlace - PosTag1 + 1)) - sStart + 1;
            if (PosTag2 > PosStartPlace) and (PosTag2 < PosEndPlace) and (PosTag2 > PosTag1) then begin
              PosTag3 := Cardinal(FBMSrchNameE.Search(@buffer[PosTag2], PosEndPlace - PosTag2 + 1)) - sStart + 1;
              if (PosTag3 > PosStartPlace) and (PosTag3 < PosEndPlace) and (PosTag3 > PosTag2) then begin
                VName := parseName(ALCopyStr(buffer, PosTag2 + 1, PosTag3 - (PosTag2 + 1)));
              end;
            end;
          end;
          VDescription := '';
          PosTag1 := Cardinal(FBMSrchDesc.Search(@buffer[PosStartPlace], PosEndPlace - PosStartPlace + 1)) - sStart + 1;
          if (PosTag1 > PosStartPlace) and (PosTag1 < PosEndPlace) then begin
            PosTag2 := Cardinal(FBMSrchCloseQ.Search(@buffer[PosTag1], PosEndPlace - PosTag1 + 1)) - sStart + 1;
            if (PosTag2 > PosStartPlace) and (PosTag2 < PosEndPlace) and (PosTag2 > PosTag1) then begin
              PosTag3 := Cardinal(FBMSrchDescE.Search(@buffer[PosTag2], PosEndPlace - PosTag2 + 1)) - sStart + 1;
              if (PosTag3 > PosStartPlace) and (PosTag3 < PosEndPlace) and (PosTag3 > PosTag2) then begin
                VDescription := parseDescription(copy(buffer, PosTag2 + 1, PosTag3 - (PosTag2 + 1)));
              end;
            end;
          end;
          PosTag1 := Cardinal(FBMSrchCoord.Search(@buffer[PosStartPlace], PosEndPlace - PosStartPlace + 1)) - sStart + 1;
          if (PosTag1 > PosStartPlace) and (PosTag1 < PosEndPlace) then begin
            PosTag2 := Cardinal(FBMSrchCloseQ.Search(@buffer[PosTag1], PosEndPlace - PosTag1 + 1)) - sStart + 1;
            if (PosTag2 > PosStartPlace) and (PosTag2 < PosEndPlace) and (PosTag2 > PosTag1) then begin
              PosTag3 := Cardinal(FBMSrchCoordE.Search(@buffer[PosTag2], PosEndPlace - PosTag2 + 1)) - sStart + 1;
              if (PosTag3 > PosStartPlace) and (PosTag3 < PosEndPlace) and (PosTag3 > PosTag2) then begin
                VPointsAggregator.Clear;
                Result := parseCoordinates(@buffer[PosTag2 + 1], PosTag3 - (PosTag2 + 1), VPointsAggregator);
              end else begin
                result := false;
              end;
            end else begin
              result := false;
            end;
          end else begin
            result := false;
          end;
        end;
        VItem := BuildItem(VName, VDescription, VPointsAggregator, AIdData, AVectorDataItemMainInfoFactory);
        if VItem <> nil then begin
          AList.Add(VItem);
        end;
      end;
      position := PosEndPlace + 1;
    except
      Result := false;
    end;
  end;
end;

function TKmlInfoSimpleParser.parseCoordinates(
  AText: PAnsiChar;
  ALen: integer;
  const APointsAggregator: IDoublePointsAggregator
): boolean;
var
  VCurPos: PAnsiChar;
  VNumEndPos: PAnsiChar;
  VComa: PAnsiChar;
  VSpace: PAnsiChar;
  VLineStart: PAnsiChar;
  VCurCoord: TDoublePoint;
  VValue: Extended;
  VLastPos: PAnsiChar;
begin
  VLineStart := AText;
  VCurPos := VLineStart;
  VLastPos := AText + ALen;
  try
    while VCurPos <> nil do begin
      VCurPos := PosOfNonSpaceChar(VCurPos, VLastPos);
      if VCurPos <> nil then begin
        VNumEndPos := PosOfChar(',', VCurPos, VLastPos);
        if VNumEndPos <> nil then begin
          VNumEndPos^ := #0;
          if TextToFloat(VCurPos, VValue, fvExtended, FFormat) then begin
            VCurCoord.x := VValue;
            VCurPos := VNumEndPos;
            Inc(VCurPos);
            if VCurPos < VLastPos then begin
              VCurPos := PosOfNonSpaceChar(VCurPos, VLastPos);
              if VCurPos <> nil then begin
                VComa := PosOfChar(',', VCurPos, VLastPos);
                VSpace := PosOfSpaceChar(VCurPos, VLastPos);
                if (VSpace <> nil) or (VComa <> nil) then begin
                  if VComa <> nil then begin
                    if (VSpace <> nil) and (VSpace < VComa) then begin
                      VNumEndPos := VSpace;
                    end else begin
                      VNumEndPos := VComa;
                    end;
                  end else begin
                    VNumEndPos := VSpace;
                  end;
                end else begin
                  VNumEndPos := VLastPos;
                end;
                VNumEndPos^ := #0;
                if TextToFloat(VCurPos, VValue, fvExtended, FFormat) then begin
                  VCurCoord.Y := VValue;
                  APointsAggregator.Add(VCurCoord);
                end;
                VCurPos := VNumEndPos;
                Inc(VCurPos);
                if VCurPos < VLastPos then begin
                  if (VComa = VNumEndPos) then begin
                    VCurPos := PosOfSpaceChar(VCurPos, VLastPos);
                  end;
                end else begin
                  VCurPos := nil;
                end;
              end;
            end else begin
              VCurPos := nil;
            end;
          end else begin
            VCurPos := VNumEndPos;
            Inc(VCurPos);
          end;
        end else begin
          VCurPos := VLastPos;
        end;
      end;
    end;
  except
    Assert(False, 'Неожиданная ошибка при разборе kml');
  end;
  Result := APointsAggregator.Count > 0;
end;

function TKmlInfoSimpleParser.PosOfChar(
  APattern: AnsiChar;
  AText: PAnsiChar;
  ALast: PAnsiChar
): PAnsiChar;
var
  VCurr: PAnsiChar;
begin
  VCurr := AText;
  Result := nil;
  while VCurr < ALast do begin
    if VCurr^ = APattern then begin
      Result := VCurr;
      Break;
    end;
    Inc(VCurr);
  end;
end;

function TKmlInfoSimpleParser.PosOfNonSpaceChar(
  AText: PAnsiChar;
  ALast: PAnsiChar
): PAnsiChar;
var
  VCurr: PAnsiChar;
begin
  VCurr := AText;
  Result := nil;
  while VCurr < ALast do begin
    if VCurr^ > ' ' then begin
      Result := VCurr;
      Break;
    end;
    Inc(VCurr);
  end;
end;

function TKmlInfoSimpleParser.PosOfSpaceChar(AText, ALast: PAnsiChar): PAnsiChar;
var
  VCurr: PAnsiChar;
begin
  VCurr := AText;
  Result := nil;
  while VCurr < ALast do begin
    if VCurr^ <= ' ' then begin
      Result := VCurr;
      Break;
    end;
    Inc(VCurr);
  end;
end;

end.
