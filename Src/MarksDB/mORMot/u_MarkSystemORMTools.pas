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
  SynCommons,
  t_GeoTypes,
  t_MarkSystemORM,
  i_GeometryLonLat;

type
  TCSVFieldsBuilder = record
    Count: Integer;
    Fields: array of RawUTF8;
    procedure Clear;
    procedure Add(const AFieldName: string);
    function Build: RawUTF8;
  end;

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
procedure LonLatDoubleRectToRect(const ADoubleRect: TDoubleRect; out ARect: TRect); inline;

const
  cCoordToInt: Cardinal = MAXLONG div 180;
  cCoordToSize: Cardinal = MAXLONG div 360;

implementation

procedure TCSVFieldsBuilder.Clear;
begin
  Finalize(Fields);
  Count := 0;
end;

procedure TCSVFieldsBuilder.Add(const AFieldName: string);
begin
  SetLength(Fields, Count + 1);
  Fields[Count] := StringToUTF8(AFieldName);
  Inc(Count);
end;

function TCSVFieldsBuilder.Build: RawUTF8;
var
  I: Integer;
  VSep: RawUTF8;
begin
  VSep := '';
  Result := '';
  for I := 0 to Length(Fields) - 1 do begin
    if I = 1 then begin
      VSep := RawUTF8(',');
    end;
    Result := Result + VSep + Fields[I];
  end;
end;

procedure CheckID(const AID: TID);
begin
  if AID = 0 then begin
    raise EMarkSystemORMError.Create('MarkSystemORM: ID is empty!');
  end;
end;

procedure CheckDeleteResult(const AResult: Boolean);
begin
  if not AResult then begin
    raise EMarkSystemORMError.Create('MarkSystemORM: Delete operation is failed!');
  end;
end;

procedure CheckUpdateResult(const AResult: Boolean);
begin
  if not AResult then begin
    raise EMarkSystemORMError.Create('MarkSystemORM: Update operation is failed!');
  end;
end;

procedure CheckRetrieveResult(const AResult: Boolean);
begin
  if not AResult then begin
    raise EMarkSystemORMError.Create('MarkSystemORM: Nothing to Retrieve!');
  end;
end;

procedure CheckExecuteResult(const AResult: Boolean);
begin
  if not AResult then begin
    raise EMarkSystemORMError.Create('MarkSystemORM: Execute operation is failed!');
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

procedure LonLatDoubleRectToRect(const ADoubleRect: TDoubleRect; out ARect: TRect);
begin
  ARect.Left := Round(ADoubleRect.Left * cCoordToInt);
  ARect.Top := Round(ADoubleRect.Top * cCoordToInt);
  ARect.Right := Round(ADoubleRect.Right * cCoordToInt);
  ARect.Bottom := Round(ADoubleRect.Bottom * cCoordToInt);
end;

end.
