unit u_KmlInfoSimpleParser;

interface

uses
  Classes,
  SysUtils,
  t_GeoTypes,
  i_VectorDataItemSimple,
  i_KmlInfoSimpleLoader,
  BMSEARCH;

type
  TKmlInfoSimpleParser = class(TInterfacedObject, IKmlInfoSimpleLoader)
  private
    FFormat: TFormatSettings;
    FBMSrchPlacemark: TSearchBM;
    FBMSrchPlacemarkE: TSearchBM;
    FBMSrchName: TSearchBM;
    FBMSrchCloseQ: TSearchBM;
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
    function parse(buffer: string; AList: IInterfaceList): boolean;
    function parseCoordinates(AText: PChar; ALen: integer; var Adata: TArrayOfDoublePoint; var ARect: TDoubleRect): boolean;
    procedure parseName(var Name: string);
    procedure parseDescription(var Description: string);
    function BuildItem(AName, ADesc: string; Adata: TArrayOfDoublePoint; ARect: TDoubleRect): IVectorDataItemSimple;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string; out AItems: IVectorDataItemList); virtual;
    procedure LoadFromStream(AStream: TStream; out AItems: IVectorDataItemList); virtual;
  end;


implementation

uses
  StrUtils,
  u_VectorDataItemPoint,
  u_VectorDataItemPolygon,
  u_VectorDataItemList,
  u_GeoFun;

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
      SetLength(Result, q - PChar(Result));
    end else begin
      Result := '';
    end;
end;


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
      Result := TVectorDataItemPoint.Create(AName, ADesc, AData[0]);
    end else begin
      if DoublePoitnsEqual(Adata[0], Adata[VPointCount - 1]) then begin
        Result := TVectorDataItemPoly.Create(AName, ADesc, Adata, ARect);
      end else begin
        Result := TVectorDataItemPath.Create(AName, ADesc, Adata, ARect);
      end;
    end;
  end;
end;

constructor TKmlInfoSimpleParser.Create;
begin
  FFormat.DecimalSeparator := '.';
  FBMSrchPlacemark := TSearchBM.Create;
  FBMSrchPlacemark.PrepareStr('<Placemark', False);
  FBMSrchPlacemarkE := TSearchBM.Create;
  FBMSrchPlacemarkE.PrepareStr('</Placemark', False);
  FBMSrchName := TSearchBM.Create;
  FBMSrchName.PrepareStr('<name', False);
  FBMSrchCloseQ := TSearchBM.Create;
  FBMSrchCloseQ.PrepareStr('>', False);
  FBMSrchNameE := TSearchBM.Create;
  FBMSrchNameE.PrepareStr('</name', False);
  FBMSrchId := TSearchBM.Create;
  FBMSrchId.PrepareStr('id=', False);
  FBMSrchCDATA := TSearchBM.Create;
  FBMSrchCDATA.PrepareStr('<![CDATA[', False);
  FBMSrchCDATAE := TSearchBM.Create;
  FBMSrchCDATAE.PrepareStr(']]>', False);
  FBMSrchDesc := TSearchBM.Create;
  FBMSrchDesc.PrepareStr('<description', False);
  FBMSrchDescE := TSearchBM.Create;
  FBMSrchDescE.PrepareStr('</description', False);
  FBMSrchLt := TSearchBM.Create;
  FBMSrchLt.PrepareStr('&lt;', False);
  FBMSrchGt := TSearchBM.Create;
  FBMSrchGt.PrepareStr('&gt;', False);
  FBMSrchCoord := TSearchBM.Create;
  FBMSrchCoord.PrepareStr('<coordinates', False);
  FBMSrchCoordE := TSearchBM.Create;
  FBMSrchCoordE.PrepareStr('</coordinates', False);
end;

destructor TKmlInfoSimpleParser.Destroy;
begin
  FreeAndNil(FBMSrchPlacemark);
  FreeAndNil(FBMSrchPlacemarkE);
  FreeAndNil(FBMSrchName);
  FreeAndNil(FBMSrchCloseQ);
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
var
  buffer: string;
  VList: IInterfaceList;
begin
  AItems := nil;
  if AStream.Size > 0 then begin
    try
      AStream.Position := 0;
      SetLength(buffer, AStream.Size);
      AStream.ReadBuffer(buffer[1], AStream.Size);
      VList := TInterfaceList.Create;
      parse(buffer, VList);
      AItems := TVectorDataItemList.Create(VList);
    finally
      SetLength(buffer, 0);
    end;
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

function TKmlInfoSimpleParser.parse(buffer: string; AList: IInterfaceList): boolean;
var
  position, PosStartPlace, PosTag1, PosTag2,PosTag3, PosEndPlace, sLen, sStart: integer;
  VName: string;
  VDescription: string;
  VPoints: TArrayOfDoublePoint;
  VRect: TDoubleRect;
  VItem: IVectorDataItemSimple;
begin
  result := true;
  buffer := Sha_SpaceCompress(buffer);
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
        end;
      VItem := BuildItem(VName, VDescription, VPoints, VRect);
      if VItem <> nil then begin
        AList.Add(VItem);
      end;
      position := PosEndPlace + 1;
    except
      Result := false;
    end;
  end;
end;

function TKmlInfoSimpleParser.parseCoordinates(AText: PChar; ALen: integer;
  var Adata: TArrayOfDoublePoint; var ARect: TDoubleRect): boolean;
var
  ii: integer;
  len: Integer;
  VCurPos: PChar;
  VNumEndPos: PChar;
  VComa: PChar;
  VSpace: PChar;
  VLineStart: PChar;
  VCurCoord: TDoublePoint;
  VAllocated: Integer;
  VUsed: Integer;
  VValue: Extended;
begin
  len := ALen;
  ii := 1;
  VUsed := 0;
  VAllocated := 32;
  SetLength(Adata, VAllocated);
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
          if TextToFloat(VCurPos, VValue, fvExtended, FFormat) then begin
            VCurCoord.x := VValue;
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
  SetLength(Adata, VUsed);
  if VUsed > 0 then begin
    ARect.TopLeft := Adata[0];
    ARect.BottomRight := Adata[0];
    for ii := 0 to length(Adata) - 1 do begin
      if ARect.Left > Adata[ii].X then begin
        ARect.Left := Adata[ii].X;
      end;
      if ARect.Right < Adata[ii].X then begin
        ARect.Right := Adata[ii].X;
      end;
      if ARect.Top < Adata[ii].y then begin
        ARect.Top := Adata[ii].y;
      end;
      if ARect.Bottom > Adata[ii].y then begin
        ARect.Bottom := Adata[ii].y;
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
