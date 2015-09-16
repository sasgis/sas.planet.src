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

unit u_ZoomArrayFunc;

interface

uses
  Types;

function IsSortedZoomArray(const AArray: TByteDynArray): Boolean;
procedure SortZoomArray(var AArray: TByteDynArray);
function ZoomArrayFromStr(const AStr: string; out AZoomArr: TByteDynArray): Boolean;
function ZoomArrayToStr(const AZoomArr: TByteDynArray): string;
function IsZoomInZoomArray(const AZoom: Byte; const AZoomArr: TByteDynArray): Boolean;
function GetZoomArrayCopy(const AZoomArr: TByteDynArray): TByteDynArray;

implementation

uses
  Classes,
  SysUtils;

const
  cUndefRangeValue = MaxInt;

function IsSortedZoomArray(const AArray: TByteDynArray): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(AArray) - 1 do begin
    if AArray[I - 1] > AArray[I] then begin
      Result := False;
      Break;
    end;
  end;
end;

procedure SortZoomArray(var AArray: TByteDynArray);

  procedure QuickSort(var AArray: TByteDynArray; L, R: Integer);
  var
    I, J: Integer;
    P: Byte;
    T: Byte;
  begin
    repeat
      I := L;
      J := R;
      P := AArray[(L + R) shr 1];
      repeat
        while AArray[I] < P do begin
          Inc(I);
        end;
        while AArray[J] > P do begin
          Dec(J);
        end;
        if I <= J then begin
          T := AArray[I];

          AArray[I] := AArray[J];
          AArray[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then begin
        QuickSort(AArray, L, J);
      end;
      L := I;
    until I >= R;
  end;

var
  VCount: Integer;
begin
  VCount := Length(AArray);
  if VCount > 1 then begin
    QuickSort(AArray, 0, VCount - 1);
  end;
end;

function ZoomArrayFromStr(const AStr: string; out AZoomArr: TByteDynArray): Boolean;
var
  I: Integer;
  VPos: Integer;
  VCount: Integer;
  VZoom, VMin, VMax: Integer;
  VList: TStringList;
begin
  VCount := 0;
  SetLength(AZoomArr, 0);

  VList := TStringList.Create;
  try
    VList.CommaText := AStr;
    for I := 0 to VList.Count - 1 do begin
      VPos := Pos('-', VList[I]);
      if VPos > 0 then begin
        if TryStrToInt(Copy(VList[I], 1, (VPos-1)), VMin) then begin
          if TryStrToInt(Copy(VList[I], (VPos+1), Length(VList[I])), VMax) then begin
            if VMin <= VMax then begin
              for VZoom := VMin to VMax do begin
                SetLength(AZoomArr, VCount+1);
                AZoomArr[VCount] := VZoom;
                Inc(VCount);
              end;
            end;
          end;
        end;
      end else begin
        if TryStrToInt(VList[I], VZoom) then begin
          SetLength(AZoomArr, VCount+1);
          AZoomArr[VCount] := VZoom;
          Inc(VCount);
        end;
      end;
    end;
  finally
    VList.Free;
  end;
  
  Result := Length(AZoomArr) > 0;

  if Result then begin
    if not IsSortedZoomArray(AZoomArr) then begin
      SortZoomArray(AZoomArr);
    end;
  end;
end;

function DiffValueIsOne(const A, B: Integer): Boolean; inline;
begin
  Result := (Abs(Abs(A) - Abs(B)) = 1);
end;

procedure FlashRange(var ARange: string; const AStart, AEnd: Integer);
var
  VFormat: string;
  VSep: string;
begin
  Assert(AStart <> cUndefRangeValue);
  Assert(AEnd <> cUndefRangeValue);

  if ARange <> '' then begin
    VSep := ',';
  end else begin
    VSep := '';
  end;

  if AStart <> AEnd then begin
    if DiffValueIsOne(AStart, AEnd) then begin
      VFormat := '%d,%d';
    end else begin
      VFormat := '%d-%d';
    end;
    ARange := ARange + VSep + Format(VFormat, [AStart, AEnd]);
  end else begin
    ARange := ARange + VSep + IntToStr(AStart);
  end;
end;

function ZoomArrayToStr(const AZoomArr: TByteDynArray): string;
var
  I: Integer;
  VArr: TByteDynArray;
  VRangeStart, VRangeEnd: Integer;
begin
  Assert(Length(AZoomArr) > 0);

  Result := '';

  if Length(AZoomArr) = 1 then begin
    Result := IntToStr(AZoomArr[0]+1);
    Exit;
  end;

  SetLength(VArr, Length(AZoomArr));
  for I := Low(AZoomArr) to High(AZoomArr) do begin
    VArr[I] := AZoomArr[I] + 1;
  end;

  if not IsSortedZoomArray(VArr) then begin
    SortZoomArray(VArr);
  end;

  VRangeStart := cUndefRangeValue;
  VRangeEnd := cUndefRangeValue;

  for I := Low(VArr) to High(VArr) do begin
    if VRangeStart = cUndefRangeValue then begin
      VRangeStart := VArr[I];
      VRangeEnd := VRangeStart;
      Continue;
    end else if DiffValueIsOne(VArr[I], VRangeEnd) then begin
      VRangeEnd := VArr[I];
      Continue;
    end;
    FlashRange(Result, VRangeStart, VRangeEnd);
    VRangeStart := VArr[I];
    VRangeEnd := VRangeStart;
  end;

  if VRangeStart <> cUndefRangeValue then begin
    FlashRange(Result, VRangeStart, VRangeEnd);
  end;
end;

function IsZoomInZoomArray(const AZoom: Byte; const AZoomArr: TByteDynArray): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(AZoomArr) to High(AZoomArr) do begin
    if AZoom = AZoomArr[I] then begin
      Result := True;
      Break;
    end;
  end;
end;

function GetZoomArrayCopy(const AZoomArr: TByteDynArray): TByteDynArray;
var
  I: Integer;
begin
  SetLength(Result, Length(AZoomArr));
  for I := Low(AZoomArr) to High(AZoomArr) do begin
    Result[I] := AZoomArr[I];
  end;
end; 

end.
