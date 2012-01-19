{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  i_HtmlToHintTextConverter,
  i_VectorItmesFactory,
  i_VectorDataItemSimple,
  i_InternalPerformanceCounter,
  i_VectorDataLoader,
  BMSEARCH;

type
  TKmlInfoSimpleParser = class(TInterfacedObject, IVectorDataLoader)
  private
    FLoadKmlStreamCounter: IInternalPerformanceCounter;
    FHintConverter: IHtmlToHintTextConverter;
    FFactory: IVectorItmesFactory;

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
    function PosOfChar(APattern: AnsiChar; AText: PAnsiChar; ALast: PAnsiChar): PAnsiChar;
    function PosOfNonSpaceChar(AText: PAnsiChar; ALast: PAnsiChar): PAnsiChar;
    function PosOfSpaceChar(AText: PAnsiChar; ALast: PAnsiChar): PAnsiChar;
    function parse(buffer: AnsiString; AList: IInterfaceList): boolean;
    function parseCoordinates(AText: PAnsiChar; ALen: integer; var Adata: TArrayOfDoublePoint; var ARect: TDoubleRect): boolean;
    procedure parseName(var Name: AnsiString);
    procedure parseDescription(var Description: AnsiString);
    function BuildItem(AName, ADesc: string; Adata: TArrayOfDoublePoint; ARect: TDoubleRect): IVectorDataItemSimple;
  protected
    procedure LoadFromFile(AFileName: string; out AItems: IVectorDataItemList); virtual;
    procedure LoadFromStream(AStream: TStream; out AItems: IVectorDataItemList); virtual;
  public
    constructor Create(
      AFactory: IVectorItmesFactory;
      AHintConverter: IHtmlToHintTextConverter;
      APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;


implementation

uses
  StrUtils,
  cUnicodeCodecs,
  u_VectorDataItemPoint,
  u_VectorDataItemPolygon,
  u_VectorDataItemList,
  u_GeoFun;

{ TKmlInfoSimpleParser }

function TKmlInfoSimpleParser.BuildItem(AName, ADesc: string;
  Adata: TArrayOfDoublePoint; ARect: TDoubleRect): IVectorDataItemSimple;
var
  VPointCount: Integer;
begin
  Result := nil;
  VPointCount := Length(Adata);
  if VPointCount > 0 then begin
    if VPointCount = 1 then begin
      Result := TVectorDataItemPoint.Create(FHintConverter, AName, ADesc, AData[0]);
    end else begin
      if DoublePointsEqual(Adata[0], Adata[VPointCount - 1]) then begin
        Result :=
          TVectorDataItemPoly.Create(
            FHintConverter,
            AName,
            ADesc,
            FFactory.CreateLonLatPolygon(@Adata[0], VPointCount),
            ARect
          );
      end else begin
        Result :=
          TVectorDataItemPath.Create(
            FHintConverter,
            AName,
            ADesc,
            FFactory.CreateLonLatPath(@Adata[0], VPointCount),
            ARect
          );
      end;
    end;
  end;
end;

constructor TKmlInfoSimpleParser.Create(
  AFactory: IVectorItmesFactory;
  AHintConverter: IHtmlToHintTextConverter;
  APerfCounterList: IInternalPerformanceCounterList
);
begin
  FFactory := AFactory;
  FHintConverter := AHintConverter;
  FLoadKmlStreamCounter := APerfCounterList.CreateAndAddNewCounter('LoadKmlStream');
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

procedure TKmlInfoSimpleParser.LoadFromFile(AFileName: string;
  out AItems: IVectorDataItemList);
var
  VFileStream: TFileStream;
begin
  VFileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(VFileStream, AItems);
  finally
    VFileStream.Free;
  end;
end;

procedure TKmlInfoSimpleParser.LoadFromStream(AStream: TStream;
   out AItems: IVectorDataItemList);

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
  VList: IInterfaceList;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FLoadKmlStreamCounter.StartOperation;
  try
    AItems := nil;
    if AStream.Size > 0 then begin
      VKml := GetAnsiString(AStream);
      if VKml <> '' then begin
        VList := TInterfaceList.Create;
        parse(VKml, VList);
        AItems := TVectorDataItemList.Create(VList);
      end else
        Assert(False, 'KML data reader - Unknown error');
    end;
  finally
    FLoadKmlStreamCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TKmlInfoSimpleParser.parseName(var Name: AnsiString);
var
  pb: integer;
begin
  Name := Utf8ToAnsi(Name);
  pb := PosEx('<![CDATA[', Name, 1);
  if pb > 0 then begin
    Name := copy(Name, pb + 9, PosEx(']]>', Name, 1) - (pb + 9));
  end;
end;

procedure TKmlInfoSimpleParser.parseDescription(var Description: AnsiString);
var
  pb: integer;
  iip: integer;
begin
  description := Utf8ToAnsi(Description);
  pb := PosEx('<![CDATA[', description, 1);
  if pb > 0 then begin
    description := copy(description, pb + 9, PosEx(']]>', description, 1) - (pb + 9));
  end;
  iip := PosEx('&lt;', description, 1);
  while iip > 0 do begin
    description[iip] := '<';
    Delete(description, iip + 1, 3);
    iip := PosEx('&lt;', description, iip);
  end;
  iip := PosEx('&gt;', description, 1);
  while iip > 0 do begin
    description[iip] := '>';
    Delete(description, iip + 1, 3);
    iip := PosEx('&gt;', description, iip);
  end;
end;

function TKmlInfoSimpleParser.parse(buffer: AnsiString; AList: IInterfaceList): boolean;
var
  position, PosStartPlace, PosTag1, PosTag2,PosTag3, PosEndPlace, sLen, sStart: integer;
  VName: string;
  VDescription: string;
  VPoints: TArrayOfDoublePoint;
  VRect: TDoubleRect;
  VItem: IVectorDataItemSimple;
begin
  result := true;
  sLen := Length(buffer);
  sStart := Integer(@buffer[1]);
  position := 1;
  PosStartPlace := 1;
  PosEndPlace := 1;
  While (position > 0) and (PosStartPlace > 0) and (PosEndPlace > 0) and (result) do begin
    try
        PosStartPlace := integer(FBMSrchPlacemark.Search(@buffer[position], sLen - position + 1)) - sStart + 1;
        if PosStartPlace > 0 then begin
          PosEndPlace := integer(FBMSrchPlacemarkE.Search(@buffer[PosStartPlace], sLen - PosStartPlace + 1)) - sStart + 1;
          if PosEndPlace > 0 then begin
            VName := '';
            position := integer(FBMSrchId.Search(@buffer[PosStartPlace], PosEndPlace - PosStartPlace + 1)) - sStart + 1;
            PosTag1 := integer(FBMSrchName.Search(@buffer[PosStartPlace], PosEndPlace - PosStartPlace + 1)) - sStart + 1;
            if (PosTag1 > PosStartPlace) and (PosTag1 < PosEndPlace) then begin
              PosTag2 := integer(FBMSrchCloseQ.Search(@buffer[PosTag1], PosEndPlace - PosTag1 + 1)) - sStart + 1;
              if (PosTag2 > PosStartPlace) and (PosTag2 < PosEndPlace) and (PosTag2 > PosTag1) then begin
                PosTag3 := integer(FBMSrchNameE.Search(@buffer[PosTag2], PosEndPlace - PosTag2 + 1)) - sStart + 1;
                if (PosTag3 > PosStartPlace) and (PosTag3 < PosEndPlace) and (PosTag3 > PosTag2) then begin
                  VName := copy(buffer, PosTag2 + 1, PosTag3 - (PosTag2 + 1));
                  parseName(VName);
                end;
              end;
            end;
            VDescription := '';
            PosTag1 := integer(FBMSrchDesc.Search(@buffer[PosStartPlace], PosEndPlace - PosStartPlace + 1)) - sStart + 1;
            if (PosTag1 > PosStartPlace) and (PosTag1 < PosEndPlace) then begin
              PosTag2 := integer(FBMSrchCloseQ.Search(@buffer[PosTag1], PosEndPlace - PosTag1 + 1)) - sStart + 1;
              if (PosTag2 > PosStartPlace) and (PosTag2 < PosEndPlace) and (PosTag2 > PosTag1) then begin
                PosTag3 := integer(FBMSrchDescE.Search(@buffer[PosTag2], PosEndPlace - PosTag2 + 1)) - sStart + 1;
                if (PosTag3 > PosStartPlace) and (PosTag3 < PosEndPlace) and (PosTag3 > PosTag2) then begin
                  Vdescription := copy(buffer, PosTag2 + 1, PosTag3 - (PosTag2 + 1));
                  parseDescription(Vdescription);
                end;
              end;
            end;
            VPoints := nil;
            PosTag1 := integer(FBMSrchCoord.Search(@buffer[PosStartPlace], PosEndPlace - PosStartPlace + 1)) - sStart + 1;
            if (PosTag1 > PosStartPlace) and (PosTag1 < PosEndPlace) then begin
              PosTag2 := integer(FBMSrchCloseQ.Search(@buffer[PosTag1], PosEndPlace - PosTag1 + 1)) - sStart + 1;
              if (PosTag2 > PosStartPlace) and (PosTag2 < PosEndPlace) and (PosTag2 > PosTag1) then begin
                PosTag3 := integer(FBMSrchCoordE.Search(@buffer[PosTag2], PosEndPlace - PosTag2 + 1)) - sStart + 1;
                if (PosTag3 > PosStartPlace) and (PosTag3 < PosEndPlace) and (PosTag3 > PosTag2) then begin
                  Result := parseCoordinates(@buffer[PosTag2 + 1], PosTag3 - (PosTag2 + 1), VPoints, VRect);
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
          VItem := BuildItem(VName, VDescription, VPoints, VRect);
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

function TKmlInfoSimpleParser.parseCoordinates(AText: PAnsiChar; ALen: integer;
  var Adata: TArrayOfDoublePoint; var ARect: TDoubleRect): boolean;
var
  VCurPos: PAnsiChar;
  VNumEndPos: PAnsiChar;
  VComa: PAnsiChar;
  VSpace: PAnsiChar;
  VLineStart: PAnsiChar;
  VCurCoord: TDoublePoint;
  VAllocated: Integer;
  VUsed: Integer;
  VValue: Extended;
  VLastPos: PAnsiChar;
  i: Integer;
begin
  VUsed := 0;
  VAllocated := 32;
  SetLength(Adata, VAllocated);
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
            if VCurPos <  VLastPos then begin
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
                  if VUsed >= VAllocated then begin
                    VAllocated := VAllocated * 2;
                    SetLength(Adata, VAllocated);
                  end;
                  Adata[VUsed] := VCurCoord;
                  Inc(VUsed);
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
        end;
      end;
    end;
  except
    Assert(False, 'Неожиданная ошибка при разборе kml');
  end;
  SetLength(Adata, VUsed);
  if VUsed > 0 then begin
    ARect.TopLeft := Adata[0];
    ARect.BottomRight := Adata[0];
    for i := 0 to length(Adata) - 1 do begin
      if ARect.Left > Adata[i].X then begin
        ARect.Left := Adata[i].X;
      end;
      if ARect.Right < Adata[i].X then begin
        ARect.Right := Adata[i].X;
      end;
      if ARect.Top < Adata[i].y then begin
        ARect.Top := Adata[i].y;
      end;
      if ARect.Bottom > Adata[i].y then begin
        ARect.Bottom := Adata[i].y;
      end;
    end;
    Result := True;
  end else begin
    result := false;
  end;
end;

function TKmlInfoSimpleParser.PosOfChar(APattern: AnsiChar; AText: PAnsiChar;
  ALast: PAnsiChar): PAnsiChar;
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

function TKmlInfoSimpleParser.PosOfNonSpaceChar(AText: PAnsiChar;
  ALast: PAnsiChar): PAnsiChar;
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
