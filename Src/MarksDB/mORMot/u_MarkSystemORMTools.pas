{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_MarkSystemORMTools;

interface

uses
  Windows,
  SysUtils,
  mORMot,
  t_GeoTypes,
  t_MarkSystemORM,
  i_GeometryLonLat;

procedure CheckID(const AID: TID); inline;
procedure CheckDeleteResult(const AResult: Boolean); inline;
procedure CheckUpdateResult(const AResult: Boolean); inline;
procedure CheckRetrieveResult(const AResult: Boolean); inline;
procedure CheckExecuteResult(const AResult: Boolean); inline;

procedure StartTransaction(
  const AClient: TSQLRestClient;
  var ATrans: TTransactionRec;
  const ASQLTableClass: TSQLRecordClass
); inline;

procedure CommitTransaction(
  const AClient: TSQLRestClient;
  var ATrans: TTransactionRec
); inline;

procedure RollBackTransaction(
  const AClient: TSQLRestClient;
  var ATrans: TTransactionRec
); inline;

function CalcMultiGeometryCount(const AGeometry: IGeometryLonLat): Integer; inline;
procedure CalcGeometrySize(const ARect: TDoubleRect; out ALonSize, ALatSize: Cardinal); inline;
procedure LonLatSizeToInternalSize(const ALonLatSize: TDoublePoint; out ALonSize, ALatSize: Cardinal); inline;

function MergeSortRemoveDuplicates(var Vals: TIDDynArray): Integer;

const
  cCoordToSize: Cardinal = MAXLONG div 360;

implementation

procedure CheckID(const AID: TID);
begin
  if AID = 0 then begin
    raise EMarkSystemORMError.Create('MarkSystemORM: ID is empty!');
  end;
end;

procedure CheckDeleteResult(const AResult: Boolean);
begin
  if not AResult then begin
    raise EMarkSystemORMError.Create('MarkSystemORM: Delete operarion is failed!');
  end;
end;

procedure CheckUpdateResult(const AResult: Boolean);
begin
  if not AResult then begin
    raise EMarkSystemORMError.Create('MarkSystemORM: Update operarion is failed!');
  end;
end;

procedure CheckRetrieveResult(const AResult: Boolean);
begin
  if not AResult then begin
    raise EMarkSystemORMError.Create('MarkSystemORM: Retrieve operarion is failed!');
  end;
end;

procedure CheckExecuteResult(const AResult: Boolean);
begin
  if not AResult then begin
    raise EMarkSystemORMError.Create('MarkSystemORM: Execute operarion is failed!');
  end;
end;

procedure StartTransaction(
  const AClient: TSQLRestClient;
  var ATrans: TTransactionRec;
  const ASQLTableClass: TSQLRecordClass
);
begin
  ATrans.FSessionID := AClient.TransactionActiveSession;
  if ATrans.FSessionID = 0 then begin
    ATrans.FSessionID := GetTickCount;
    if not AClient.TransactionBegin(ASQLTableClass, ATrans.FSessionID) then begin
      raise EMarkSystemORMError.Create('MarkSystemORM: Start transaction is failed!');
    end;
    ATrans.FIsInternal := True;
  end else begin
    ATrans.FIsInternal := False;
  end;
end;

procedure CommitTransaction(
  const AClient: TSQLRestClient;
  var ATrans: TTransactionRec
);
begin
  Assert(ATrans.FSessionID > 0);
  if ATrans.FIsInternal and (ATrans.FSessionID > 0) then begin
    AClient.Commit(ATrans.FSessionID, True);
    ATrans.FSessionID := 0;
  end;
end;

procedure RollBackTransaction(
  const AClient: TSQLRestClient;
  var ATrans: TTransactionRec
);
begin
  Assert(ATrans.FSessionID > 0);
  if ATrans.FIsInternal and (ATrans.FSessionID > 0) then begin
    AClient.RollBack(ATrans.FSessionID);
    ATrans.FSessionID := 0;
  end;
end;

function CalcMultiGeometryCount(const AGeometry: IGeometryLonLat): Integer;
var
  VLine: IGeometryLonLatMultiLine;
  VPoly: IGeometryLonLatMultiPolygon;
begin
  if Supports(AGeometry, IGeometryLonLatMultiLine, VLine) then begin
    Result := VLine.Count;
  end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VPoly) then begin
    Result := VPoly.Count;
  end else begin
    Result := 1;
  end;
end;

procedure CalcGeometrySize(const ARect: TDoubleRect; out ALonSize, ALatSize: Cardinal);
var
  VLonLatSize: TDoublePoint;
begin
  if (ARect.Right = ARect.Left) and (ARect.Top = ARect.Bottom) then begin
    ALonSize := 0;
    ALatSize := 0;
  end else begin
    VLonLatSize.X := ARect.Right - ARect.Left;
    VLonLatSize.Y := ARect.Top - ARect.Bottom;
    LonLatSizeToInternalSize(VLonLatSize, ALonSize, ALatSize);
  end;
end;

procedure LonLatSizeToInternalSize(const ALonLatSize: TDoublePoint; out ALonSize, ALatSize: Cardinal);
begin
  ALonSize := Round(ALonLatSize.X * cCoordToSize);
  ALatSize := Round(ALonLatSize.Y * cCoordToSize);
end;

function MergeSortRemoveDuplicates(var Vals: TIDDynArray): Integer;
// Mergesort modification with duplicate deleting.
// returns new valid length
var
  AVals: TIDDynArray;

   //returns index of the last valid element
  function Merge(I0, I1, J0, J1: Integer): Integer;
  var
    i, j, k, LC: Integer;
  begin
    LC := I1 - I0;
    for i := 0 to LC do
      AVals[i]:=Vals[i + I0];
      //copy lower half or Vals into temporary array AVals

    k := I0;
    i := 0;
    j := J0;
    while ((i <= LC) and (j <= J1)) do
    if (AVals[i] < Vals[j]) then begin
      Vals[k] := AVals[i];
      inc(i);
      inc(k);
    end else  if (AVals[i] > Vals[j]) then begin
      Vals[k]:=Vals[j];
      inc(k);
      inc(j);
    end else begin //duplicate
      Vals[k] := AVals[i];
      inc(i);
      inc(j);
      inc(k);
    end;

    //copy the rest
    while i <= LC do begin
      Vals[k] := AVals[i];
      inc(i);
      inc(k);
    end;

    if k <> j then
      while j <= J1 do begin
        Vals[k]:=Vals[j];
        inc(k);
        inc(j);
      end;

    Result := k - 1;
  end;

  //returns index of the last valid element
  function PerformMergeSort(ALo, AHi: Integer): Integer;
  var
    AMid, I1, J1: Integer;
  begin
    //It would be wise to use Insertion Sort when (AHi - ALo) is small (about 32-100)
    if ALo < AHi then begin
      AMid:=(ALo + AHi) shr 1;
      I1 := PerformMergeSort(ALo, AMid);
      J1 := PerformMergeSort(AMid + 1, AHi);
      Result := Merge(ALo, I1, AMid + 1, J1);
    end else begin
      Result := ALo;
    end;
  end;

begin
  SetLength(AVals, Length(Vals) div 2 + 1);
  Result := 1 + PerformMergeSort(0, High(Vals));
end;

end.
