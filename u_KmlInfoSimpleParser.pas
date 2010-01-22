unit u_KmlInfoSimpleParser;

interface

uses
  Classes,
  SysUtils,
  t_GeoTypes,
  u_KmlInfoSimple,
  i_IKmlInfoSimpleLoader,
  BMSEARCH;

type
  TKmlInfoSimpleParser = class(TInterfacedObject, IKmlInfoSimpleLoader)
  private
    FFormat: TFormatSettings;
    FBMSrchPlacemark: TSearchBM;
    FBMSrchPlacemarkE: TSearchBM;
    FBMSrchName: TSearchBM;
    FBMSrchNameE: TSearchBM;
    FBMSrchId: TSearchBM;
    FBMSrchCDATA: TSearchBM;
    FBMSrchCDATAE: TSearchBM;
    FBMSrchDesc: TSearchBM;
    FBMSrchDescE: TSearchBM;
    FBMSrchLt: TSearchBM;
    FBMSrchGt: TSearchBM;
    FBMSrchCoord: TSearchBM;
    FBMSrchCoordE: TSearchBM;
    function PosOfChar(APattern: Char; AText: PChar; ALen: Integer): PChar;
    function parse(buffer: string; ABtm: TKmlInfoSimple): boolean;
    function parseCoordinates(AText: PChar; ALen: integer; var Adata: TKMLData): boolean;
    procedure parseName(var Name: string);
    procedure parseDescription(var Description: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string; ABtm: TKmlInfoSimple);
    procedure LoadFromStream(AStream: TStream; ABtm: TKmlInfoSimple);
  end;


implementation

uses
  StrUtils;

function Sha_SpaceCompress(const s: string): string;
var
  p, q, t: pchar;
  ch: char;
  len: Integer;
label
  rt;
begin
  p := PChar(s);
  q := nil;
  if p <> nil then begin
    len := length(s);
    t := p + len;
    if p < t then begin
      repeat
        dec(t);
        if p > t then begin
          goto rt;
        end;
      until (t^ > ' ');
      SetString(Result, nil, (t - p) + 1);
      q := pchar(Result);
      repeat
        repeat
          ch := p^;
          inc(p);
        until ch > ' ';
        repeat
          q^ := ch;
          ch := p^;
          inc(q);
          inc(p);
        until ch <= ' ';
        q^ := ' ';
        inc(q);
      until p > t;
    end;
  end;
  rt:
    if q <> nil then begin
      dec(q);
      q^ := #0;
      (pinteger(pchar(pointer(Result)) - 4))^ := q - pointer(Result);
    end else begin
      Result := '';
    end;
end;


{ TKmlInfoSimpleParser }

constructor TKmlInfoSimpleParser.Create;
begin
  FFormat.DecimalSeparator := '.';
  FBMSrchPlacemark:=TSearchBM.Create;
  FBMSrchPlacemark.PrepareStr('<Placemark',False);
  FBMSrchPlacemarkE:=TSearchBM.Create;
  FBMSrchPlacemarkE.PrepareStr('</Placemark',False);
  FBMSrchName:=TSearchBM.Create;
  FBMSrchName.PrepareStr('<name',False);
  FBMSrchNameE:=TSearchBM.Create;
  FBMSrchNameE.PrepareStr('</name',False);
  FBMSrchId:=TSearchBM.Create;
  FBMSrchId.PrepareStr('id=',False);
  FBMSrchCDATA:=TSearchBM.Create;
  FBMSrchCDATA.PrepareStr('<![CDATA[',False);
  FBMSrchCDATAE:=TSearchBM.Create;
  FBMSrchCDATAE.PrepareStr(']]>',False);
  FBMSrchDesc:=TSearchBM.Create;
  FBMSrchDesc.PrepareStr('<description',False);
  FBMSrchDescE:=TSearchBM.Create;
  FBMSrchDescE.PrepareStr('</description',False);
  FBMSrchLt:=TSearchBM.Create;
  FBMSrchLt.PrepareStr('&lt;',False);
  FBMSrchGt:=TSearchBM.Create;
  FBMSrchGt.PrepareStr('&gt;',False);
  FBMSrchCoord:=TSearchBM.Create;
  FBMSrchCoord.PrepareStr('<coordinates',False);
  FBMSrchCoordE:=TSearchBM.Create;
  FBMSrchCoordE.PrepareStr('</coordinates',False);
end;

destructor TKmlInfoSimpleParser.Destroy;
begin
  FreeAndNil(FBMSrchPlacemark);
  FreeAndNil(FBMSrchPlacemarkE);
  FreeAndNil(FBMSrchName);
  FreeAndNil(FBMSrchNameE);
  FreeAndNil(FBMSrchId);
  FreeAndNil(FBMSrchCDATA);
  FreeAndNil(FBMSrchCDATAE);
  FreeAndNil(FBMSrchDesc);
  FreeAndNil(FBMSrchDescE);
  FreeAndNil(FBMSrchLt);
  FreeAndNil(FBMSrchGt);
  FreeAndNil(FBMSrchCoord);
  FreeAndNil(FBMSrchCoordE);
  inherited;
end;

procedure TKmlInfoSimpleParser.LoadFromFile(AFileName: string;
  ABtm: TKmlInfoSimple);
var
  VFileStream: TFileStream;
begin
  VFileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(VFileStream, ABtm);
  finally
    VFileStream.Free;
  end;
end;

procedure TKmlInfoSimpleParser.LoadFromStream(AStream: TStream;
  ABtm: TKmlInfoSimple);
var
  buffer: string;
begin
  try
    AStream.Position := 0;
    SetLength(buffer, AStream.Size);
    AStream.ReadBuffer(buffer[1], AStream.Size);
    parse(buffer, ABtm);
  finally
    SetLength(buffer, 0);
  end;
end;

procedure TKmlInfoSimpleParser.parseName(var Name: string);
var
  pb: integer;
begin
  Name := Utf8ToAnsi(Name);
  pb := PosEx('<![CDATA[', Name, 1);
  if pb > 0 then begin
    Name := copy(Name, pb + 9, PosEx(']]>', Name, 1) - (pb + 9));
  end;
end;

procedure TKmlInfoSimpleParser.parseDescription(var Description: string);
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

function TKmlInfoSimpleParser.parse(buffer: string; ABtm: TKmlInfoSimple): boolean;
var
  position, PosStartPlace, PosTag1, PosTag2, PosEndPlace, placeN, sLen,sStart: integer;
begin
  result := true;
  buffer := Sha_SpaceCompress(buffer);
  sLen:=Length(buffer);
  sStart:=Integer(@buffer[1]);
  position := 1;
  PosStartPlace := 1;
  PosEndPlace := 1;
  placeN := 0;
  While (position > 0) and (PosStartPlace > 0) and (PosEndPlace > 0) and (result) do begin
    try
      SetLength(ABtm.Data, placeN + 1);
      With ABtm.Data[PlaceN] do begin
        PosStartPlace := integer(FBMSrchPlacemark.Search(@buffer[position],sLen-position+1))-sStart+1;
        if PosStartPlace > 0 then begin
          PosEndPlace := integer(FBMSrchPlacemarkE.Search(@buffer[PosStartPlace],sLen-PosStartPlace+1))-sStart+1;
          if PosEndPlace > 0 then begin
            position := integer(FBMSrchId.Search(@buffer[PosStartPlace],PosEndPlace-PosStartPlace+1))-sStart+1;
            if (position < PosEndPlace) and (position > PosStartPlace) then begin
              PlacemarkID := copy(buffer, position + 6, PosEx('">', buffer, position) - (position + 6));
            end else begin
              PlacemarkID := '';
            end;
            PosTag1 := integer(FBMSrchName.Search(@buffer[PosStartPlace],PosEndPlace-PosStartPlace+1))-sStart+1;
            if (PosTag1 > PosStartPlace) and (PosTag1 < PosEndPlace) then begin
              PosTag2 := integer(FBMSrchNameE.Search(@buffer[PosTag1],PosEndPlace-PosTag1+1))-sStart+1;
              if (PosTag2 > PosStartPlace) and (PosTag2 < PosEndPlace) and (PosTag2 > PosTag1) then begin
                Name := copy(buffer, PosTag1 + 6, PosTag2 - (PosTag1 + 6));
                parseName(Name);
              end else begin
                Name := '';
              end;
            end else begin
              Name := '';
            end;
            PosTag1 := integer(FBMSrchDesc.Search(@buffer[PosStartPlace],PosEndPlace-PosStartPlace+1))-sStart+1;
            if (PosTag1 > PosStartPlace) and (PosTag1 < PosEndPlace) then begin
              PosTag2 := integer(FBMSrchDescE.Search(@buffer[PosTag1],PosEndPlace-PosTag1+1))-sStart+1;
              if (PosTag2 > PosStartPlace) and (PosTag2 < PosEndPlace) and (PosTag2 > PosTag1) then begin
                description := copy(buffer, PosTag1 + 13, PosTag2 - (PosTag1 + 13));
                parseDescription(description);
              end else begin
                description := '';
              end;
            end else begin
              description := '';
            end;
            PosTag1 := integer(FBMSrchCoord.Search(@buffer[PosStartPlace],PosEndPlace-PosStartPlace+1))-sStart+1;
            if (PosTag1 > PosStartPlace) and (PosTag1 < PosEndPlace) then begin
              PosTag2 := integer(FBMSrchCoordE.Search(@buffer[PosTag1],PosEndPlace-PosTag1+1))-sStart+1;
              if (PosTag2 > PosStartPlace) and (PosTag2 < PosEndPlace) and (PosTag2 > PosTag1) then begin
                Result := parseCoordinates(@buffer[PosTag1 + 13],PosTag2 - (PosTag1 + 13), ABtm.Data[PlaceN]);
              end else begin
                result := false;
              end;
            end else begin
              result := false;
            end;
          end;
        end;
      end;
      inc(placeN);
      position := PosEndPlace + 1;
    except
      Result := false;
    end;
  end;
  SetLength(ABtm.Data, length(ABtm.Data) - 1);
end;

function TKmlInfoSimpleParser.parseCoordinates(AText: PChar; ALen: integer;
  var Adata: TKMLData): boolean;
var
  ii: integer;
  len: Integer;
  VCurPos: PChar;
  VNumEndPos: PChar;
  VComa: PChar;
  VSpace: PChar;
  VLineStart: PChar;
  VCurCoord: TExtendedPoint;
  VAllocated: Integer;
  VUsed: Integer;
begin
  len := ALen;
  ii := 1;
  VUsed := 0;
  VAllocated := 32;
  SetLength(Adata.coordinates, VAllocated);
  VLineStart := AText;
  VCurPos := VLineStart;
  try
    while ii <= len do begin
      if VCurPos^ = ' ' then begin
        inc(VCurPos);
        inc(ii);
      end;
      if ii <= len then begin
        VNumEndPos := PosOfChar(',', VCurPos, len - ii + 1);
        if VNumEndPos <> nil then begin
          VNumEndPos^ := #0;
          if TextToFloat(VCurPos, VCurCoord.x, fvExtended, FFormat) then begin
            VCurPos := VNumEndPos;
            Inc(VCurPos);
            ii := VCurPos - VLineStart + 1;
            if VCurPos^ = ' ' then begin
              inc(ii);
              inc(VCurPos);
            end;
            VComa := PosOfChar(',', VCurPos, len - ii + 1);
            VSpace := PosOfChar(' ', VCurPos, len - ii + 1);
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
              VNumEndPos := VLineStart + Len;
            end;
            VNumEndPos^ := #0;
            if TextToFloat(VCurPos, VCurCoord.y, fvExtended, FFormat) then begin
              if VUsed >= VAllocated then begin
                VAllocated := VAllocated * 2;
                SetLength(Adata.coordinates, VAllocated);
              end;
              Adata.coordinates[VUsed] := VCurCoord;
              Inc(VUsed);
            end;
            VCurPos := VNumEndPos;
            Inc(VCurPos);
            ii := VCurPos - VLineStart + 1;
            if (VComa = VNumEndPos) then begin
              while ((VCurPos^ in ['0'..'9', 'e', 'E', '.', '-'])) do begin
                inc(ii);
                inc(VCurPos);
              end;
            end;
          end else begin
            VCurPos := VNumEndPos;
            Inc(VCurPos);
            ii := VCurPos - VLineStart + 1;
          end;
        end else begin
          ii := len + 1;
        end;
      end;
    end;
  except
    Assert(False, 'Неожиданная ошибка при разборе kml');
  end;
  SetLength(Adata.coordinates, VUsed);
  if VUsed > 0 then begin
    Adata.coordinatesLT := Adata.coordinates[0];
    Adata.coordinatesRD := Adata.coordinates[0];
    for ii := 0 to length(Adata.coordinates) - 1 do begin
      if Adata.coordinatesLT.x > Adata.coordinates[ii].X then begin
        Adata.coordinatesLT.x := Adata.coordinates[ii].X;
      end;
      if Adata.coordinatesRD.x < Adata.coordinates[ii].X then begin
        Adata.coordinatesRD.x := Adata.coordinates[ii].X;
      end;
      if Adata.coordinatesLT.y < Adata.coordinates[ii].y then begin
        Adata.coordinatesLT.y := Adata.coordinates[ii].y;
      end;
      if Adata.coordinatesRD.y > Adata.coordinates[ii].y then begin
        Adata.coordinatesRD.y := Adata.coordinates[ii].y;
      end;
    end;
    Result := True;
  end else begin
    result := false;
  end;
end;

function TKmlInfoSimpleParser.PosOfChar(APattern: Char; AText: PChar;
  ALen: Integer): PChar;
var
  i: integer;
  VCurr: PChar;
begin
  i := 0;
  VCurr := AText;
  Result := nil;
  while i < ALen do begin
    if VCurr^ = APattern then begin
      Result := VCurr;
      Break;
    end;
    Inc(VCurr);
    Inc(i);
  end;
end;

end.
