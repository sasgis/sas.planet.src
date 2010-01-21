unit UKmlParse;

interface

uses
  Classes,
  t_GeoTypes;

type
  TKMLData = record
    PlacemarkID: string;
    Name: string;
    description: string;
    coordinates: TExtendedPointArray;
    coordinatesLT: TExtendedPoint;
    coordinatesRD: TExtendedPoint;
  end;

  TKML = class
  private
    Error_: string;
    function parse(buffer: string): boolean;
    function parseCoordinates(var koord: string; var Adata: TKMLData): boolean;
  public
    Data: Array of TKMLData;
    constructor Create;
    destructor Destroy; override;
    function loadFromFile(FileName: string): boolean;
    function loadFromStream(str: TStream): boolean;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  UResStrings;

function Sha_SpaceCompress(const s: string): string;
var
  p, q, t: pchar;
  ch: char;
label
  rt;
begin
  p := pointer(s);
  q := nil;
  if p <> nil then begin
    t := p + (pinteger(p - 4))^;
    if p < t then begin
      repeat
        ;
        dec(t);
        if p > t then begin
          goto rt;
        end;
      until (t^ > ' ');
      SetString(Result, nil, (t - p) + 1);
      q := pchar(pointer(Result));
      repeat
        ;
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

constructor TKML.Create;
begin
  Data := nil;
  Error_ := '';
end;

destructor TKML.Destroy;
var
  i: integer;
begin
  Error_ := '';
  if Data <> nil then begin
    for i := 0 to Length(Data) - 1 do begin
      Data[i].PlacemarkID := '';
      Data[i].Name := '';
      Data[i].description := '';
      Data[i].coordinates := nil;
    end;
  end;
  inherited;
end;

function TKML.loadFromFile(FileName: string): boolean;
var
  buffer: string;
  str: TMemoryStream;
begin
  error_ := '';
  if not (FileExists(FileName)) then begin
    result := false;
    error_ := SAS_ERR_FileNotFound;
    exit;
  end;
  try
    str := TMemoryStream.Create;
    try
      str.LoadFromFile(FileName);
      str.Position := 0;
      SetLength(buffer, str.Size);
      str.ReadBuffer(buffer[1], str.Size);
      result := parse(buffer);
    finally
      str.Free;
      SetLength(buffer, 0);
    end;
  except
    result := false;
  end;
end;

function TKML.loadFromStream(str: TStream): boolean;
var
  buffer: string;
begin
  error_ := '';
  try
    try
      str.Position := 0;
      SetLength(buffer, str.Size);
      str.ReadBuffer(buffer[1], str.Size);
      result := parse(buffer);
    finally
      SetLength(buffer, 0);
    end;
  except
    result := false;
  end;
end;

function TKML.parse(buffer: string): boolean;
var
  koord: string;
  position, PosStartPlace, PosTag1, PosTag2, PosEndPlace, placeN, iip: integer;
  pb: integer;
begin
  result := true;
  error_ := '';
  buffer := Sha_SpaceCompress(buffer);
  position := 1;
  PosStartPlace := 1;
  PosEndPlace := 1;
  placeN := 0;
  Data := nil;
  While (position > 0) and (PosStartPlace > 0) and (PosEndPlace > 0) and (result) do begin
    try
      SetLength(Data, placeN + 1);
      With Data[PlaceN] do begin
        PosStartPlace := PosEx('<Placemark', buffer, position);
        if PosStartPlace < 1 then begin
          continue;
        end;
        PosEndPlace := PosEx('</Placemark', buffer, PosStartPlace);
        if PosEndPlace < 1 then begin
          continue;
        end;
        position := PosEx('id=', buffer, PosStartPlace);
        if (position < PosEndPlace) and (position > PosStartPlace) then begin
          PlacemarkID := copy(buffer, position + 6, PosEx('">', buffer, position) - (position + 6));
        end else begin
          PlacemarkID := '';
        end;
        PosTag1 := PosEx('<name', buffer, PosStartPlace);
        if (PosTag1 > PosStartPlace) and (PosTag1 < PosEndPlace) then begin
          PosTag2 := PosEx('</name', buffer, PosTag1);
          if (PosTag2 > PosStartPlace) and (PosTag2 < PosEndPlace) and (PosTag2 > PosTag1) then begin
            Name := Utf8ToAnsi(copy(buffer, PosTag1 + 6, PosTag2 - (PosTag1 + 6)));
            pb := PosEx('<![CDATA[', Name, 1);
            if pb > 0 then begin
              Name := copy(Name, pb + 9, PosEx(']]>', Name, 1) - (pb + 9));
            end;
          end else begin
            Name := '';
          end;
        end else begin
          Name := '';
        end;
        PosTag1 := PosEx('<description', buffer, PosStartPlace);
        if (PosTag1 > PosStartPlace) and (PosTag1 < PosEndPlace) then begin
          PosTag2 := PosEx('</description', buffer, PosTag1);
          if (PosTag2 > PosStartPlace) and (PosTag2 < PosEndPlace) and (PosTag2 > PosTag1) then begin
            description := Utf8ToAnsi(copy(buffer, PosTag1 + 13, PosTag2 - (PosTag1 + 13)));
            pb := PosEx('<![CDATA[', description, 1);
            if pb > 0 then begin
              Data[PlaceN].description := copy(description, pb + 9, PosEx(']]>', description, 1) - (pb + 9));
            end;
            iip := PosEx('&lt;', Data[PlaceN].description, 1);
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
          end else begin
            Data[PlaceN].description := '';
          end;
        end else begin
          Data[PlaceN].description := '';
        end;
        PosTag1 := PosEx('<coordinates', buffer, PosStartPlace);
        if (PosTag1 > PosStartPlace) and (PosTag1 < PosEndPlace) then begin
          PosTag2 := PosEx('</coordinates', buffer, PosTag1);
          if (PosTag2 > PosStartPlace) and (PosTag2 < PosEndPlace) and (PosTag2 > PosTag1) then begin
            koord := copy(buffer, PosTag1 + 13, PosTag2 - (PosTag1 + 13));
            Result := parseCoordinates(koord, Data[PlaceN]);
          end else begin
            result := false;
          end;
        end else begin
          result := false;
        end;
      end;
      inc(placeN);
      position := PosEndPlace + 1;
    except
      Result := false;
      error_ := SAS_ERR_Read;
    end;
  end;
  SetLength(Data, length(Data) - 1);
end;

function TKML.parseCoordinates(var koord: string; var Adata: TKMLData): boolean;
var
  VFormat: TFormatSettings;
  ii, iip: integer;
  iip_: integer;
  len: Integer;
  VCurPos: PChar;
  VNumEndPos: PChar;
  VCurCoord: TExtendedPoint;
  VAllocated: Integer;
  VUsed: Integer;
begin
  VFormat.DecimalSeparator := '.';
  len := length(koord);
  ii := 1;
  VUsed := 0;
  VAllocated := 32;
  SetLength(Adata.coordinates, VAllocated);
  VCurPos := PChar(koord);
  try
    while ii <= len do begin
      if VCurPos^ = ' ' then begin
        inc(VCurPos);
        inc(ii);
      end;
      if ii > len then begin
        continue;
      end;
      iip := posEx(',', koord, ii);
      VNumEndPos := VCurPos;
      Inc(VNumEndPos, iip - ii);
      VNumEndPos^ := #0;
      if TextToFloat(VCurPos, VCurCoord.x, fvExtended, VFormat) then begin
        VCurPos := VNumEndPos;
        Inc(VCurPos);
        ii := iip + 1;
        if VCurPos^ = ' ' then begin
          inc(ii);
          inc(VCurPos);
        end;
        iip := posEx(',', koord, ii);
        iip_ := posEx(' ', koord, ii);
        if (iip_ > 0) and (iip_ < iip) then begin
          iip := iip_;
        end;
        if iip = 0 then begin
          iip := Len + 1;
        end;
        VNumEndPos := VCurPos;
        Inc(VNumEndPos, iip - ii);
        VNumEndPos^ := #0;
        if TextToFloat(VCurPos, VCurCoord.y, fvExtended, VFormat) then begin
          if VUsed >= VAllocated then begin
            VAllocated := VAllocated * 2;
            SetLength(Adata.coordinates, VAllocated);
          end;
          Adata.coordinates[VUsed] := VCurCoord;
          Inc(VUsed);
        end;
        VCurPos := VNumEndPos;
        Inc(VCurPos);
        ii := iip + 1;
        if (iip <> iip_) then begin
          while ((VCurPos^ in ['0'..'9', 'e', 'E', '.', '-'])) do begin
            inc(ii);
            inc(VCurPos);
          end;
        end;
      end else begin
        Inc(VCurPos, iip + 1 - ii);
        ii := iip + 1;
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

end.
