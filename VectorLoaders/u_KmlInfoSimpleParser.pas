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

unit u_KmlInfoSimpleParser;

interface

uses
  Classes,
  SysUtils,
  t_GeoTypes,
  i_BinaryData,
  i_InterfaceListSimple,
  i_VectorDataFactory,
  i_VectorItemsFactory,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_DoublePointsAggregator,
  i_InternalPerformanceCounter,
  i_VectorDataLoader,
  BMSEARCH,
  u_BaseInterfacedObject;

type
  TKmlInfoSimpleParser = class(TBaseInterfacedObject, IVectorDataLoader)
  private
    FLoadKmlStreamCounter: IInternalPerformanceCounter;
    FFactory: IVectorItemsFactory;

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
      const AList: IInterfaceListSimple;
      const AIdData: Pointer;
      const AFactory: IVectorDataFactory
    ): boolean;
    function parseCoordinates(
      AText: PAnsiChar;
      ALen: integer;
      const APointsAggregator: IDoublePointsAggregator
    ): boolean;
    procedure parseName(var Name: String);
    procedure parseDescription(var Description: String);
    function BuildItem(
      const AName, ADesc: string;
      const APointsAggregator: IDoublePointsAggregator;
      const AIdData: Pointer;
      const AFactory: IVectorDataFactory
    ): IVectorDataItemSimple;
    function LoadFromStreamInternal(
      AStream: TStream;
      const AIdData: Pointer;
      const AFactory: IVectorDataFactory
    ): IVectorItemSubset;
  private
    function LoadFromStream(
      AStream: TStream;
      const AIdData: Pointer;
      const AFactory: IVectorDataFactory
    ): IVectorItemSubset;
    function Load(
      const AData: IBinaryData;
      const AIdData: Pointer;
      const AFactory: IVectorDataFactory
    ): IVectorItemSubset;
  public
    constructor Create(
      const AFactory: IVectorItemsFactory;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;

implementation

uses
  StrUtils,
  cUnicodeCodecs,
  u_InterfaceListSimple,
  u_StreamReadOnlyByBinaryData,
  u_DoublePointsAggregator,
  u_VectorDataItemSubset,
  u_GeoFun;

{ TKmlInfoSimpleParser }

function TKmlInfoSimpleParser.BuildItem(
  const AName, ADesc: string;
  const APointsAggregator: IDoublePointsAggregator;
  const AIdData: Pointer;
  const AFactory: IVectorDataFactory
): IVectorDataItemSimple;
var
  VPointCount: Integer;
begin
  Result := nil;
  VPointCount := APointsAggregator.Count;
  if VPointCount > 0 then begin
    if VPointCount = 1 then begin
      Result := AFactory.BuildPoint(AIdData, AName, ADesc, APointsAggregator.Points[0]);
    end else begin
      if DoublePointsEqual(APointsAggregator.Points[0], APointsAggregator.Points[VPointCount - 1]) then begin
        Result :=
          AFactory.BuildPoly(
            AIdData,
            AName,
            ADesc,
            FFactory.CreateLonLatPolygon(APointsAggregator.Points, VPointCount)
          );
      end else begin
        Result :=
          AFactory.BuildPath(
            AIdData,
            AName,
            ADesc,
            FFactory.CreateLonLatPath(APointsAggregator.Points, VPointCount)
          );
      end;
    end;
  end;
end;

constructor TKmlInfoSimpleParser.Create(
  const AFactory: IVectorItemsFactory;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FFactory := AFactory;
  if APerfCounterList <> nil then begin
    FLoadKmlStreamCounter := APerfCounterList.CreateAndAddNewCounter('LoadKmlStream');
  end;
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
  const AFactory: IVectorDataFactory
): IVectorItemSubset;
var
  VStream: TStreamReadOnlyByBinaryData;
begin
  Result := nil;
  VStream := TStreamReadOnlyByBinaryData.Create(AData);
  try
    Result := LoadFromStream(VStream, AIdData, AFactory);
  finally
    VStream.Free;
  end;
end;

function TKmlInfoSimpleParser.LoadFromStream(
  AStream: TStream;
  const AIdData: Pointer;
  const AFactory: IVectorDataFactory
): IVectorItemSubset;
var
  VCounterContext: TInternalPerformanceCounterContext;
begin
  if FLoadKmlStreamCounter <> nil then begin
    VCounterContext := FLoadKmlStreamCounter.StartOperation;
    try
      Result := LoadFromStreamInternal(AStream, AIdData, AFactory);
    finally
      FLoadKmlStreamCounter.FinishOperation(VCounterContext);
    end;
  end else begin
    Result := LoadFromStreamInternal(AStream, AIdData, AFactory);
  end;
end;

function TKmlInfoSimpleParser.LoadFromStreamInternal(AStream: TStream;
  const AIdData: Pointer;
  const AFactory: IVectorDataFactory
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
  VList: IInterfaceListSimple;
begin
  Result := nil;
  if AStream.Size > 0 then begin
    VKml := GetAnsiString(AStream);
    if VKml <> '' then begin
      VList := TInterfaceListSimple.Create;
      parse(VKml, VList, AIdData, AFactory);
      Result := TVectorItemSubset.Create(VList.MakeStaticAndClear);
    end else begin
      Assert(False, 'KML data reader - Unknown error');
    end;
  end;
end;

procedure TKmlInfoSimpleParser.parseName(var Name: String);
var
  pb: integer;
begin
  Name := Utf8ToAnsi(Name);
  pb := PosEx('<![CDATA[', Name, 1);
  if pb > 0 then begin
    Name := copy(Name, pb + 9, PosEx(']]>', Name, 1) - (pb + 9));
  end;
end;

procedure TKmlInfoSimpleParser.parseDescription(var Description: String);
var
  pb: integer;
  iip: integer;
begin
  Description := Utf8ToAnsi(Description);
  pb := PosEx('<![CDATA[', Description, 1);
  if pb > 0 then begin
    Description := copy(Description, pb + 9, PosEx(']]>', Description, 1) - (pb + 9));
  end;
  iip := PosEx('&lt;', Description, 1);
  while iip > 0 do begin
    Description[iip] := '<';
    Delete(Description, iip + 1, 3);
    iip := PosEx('&lt;', Description, iip);
  end;
  iip := PosEx('&gt;', Description, 1);
  while iip > 0 do begin
    Description[iip] := '>';
    Delete(Description, iip + 1, 3);
    iip := PosEx('&gt;', Description, iip);
  end;
end;

function TKmlInfoSimpleParser.parse(
  const buffer: AnsiString;
  const AList: IInterfaceListSimple;
  const AIdData: Pointer;
  const AFactory: IVectorDataFactory
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
                VName := copy(buffer, PosTag2 + 1, PosTag3 - (PosTag2 + 1));
                parseName(VName);
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
                VDescription := copy(buffer, PosTag2 + 1, PosTag3 - (PosTag2 + 1));
                parseDescription(VDescription);
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
        VItem := BuildItem(VName, VDescription, VPointsAggregator, AIdData, AFactory);
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
