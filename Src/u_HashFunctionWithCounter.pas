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

unit u_HashFunctionWithCounter;

interface

uses
  Types,
  t_Hash,
  t_GeoTypes,
  i_HashFunction,
  i_HashFunctionImpl,
  i_InternalPerformanceCounter,
  u_BaseInterfacedObject;

type
  THashFunctionWithCounter = class(TBaseInterfacedObject, IHashFunction)
  private
    FFunction: IHashFunctionImpl;
    FCounterSmallString: IInternalPerformanceCounter;
    FCounterBigString: IInternalPerformanceCounter;
    FCounterGUID: IInternalPerformanceCounter;
    FCounterDouble: IInternalPerformanceCounter;
    FCounterInteger: IInternalPerformanceCounter;
    FCounterPoint: IInternalPerformanceCounter;
    FCounterDoublePoint: IInternalPerformanceCounter;
    FCounterRect: IInternalPerformanceCounter;
    FCounterDoubleRect: IInternalPerformanceCounter;
    FCounterSmallBuffer: IInternalPerformanceCounter;
    FCounterBigBuffer: IInternalPerformanceCounter;
    FCounterHash: IInternalPerformanceCounter;
  private
    function CalcHashByString(const AValue: string): THashValue;
    function CalcHashByAnsiString(const AValue: AnsiString): THashValue;
    function CalcHashByGUID(const AValue: TGUID): THashValue;
    function CalcHashByDouble(const AValue: Double): THashValue;
    function CalcHashByInteger(const AValue: Integer): THashValue;
    function CalcHashByPoint(const AValue: TPoint): THashValue;
    function CalcHashByDoublePoint(const AValue: TDoublePoint): THashValue;
    function CalcHashByRect(const AValue: TRect): THashValue;
    function CalcHashByDoubleRect(const AValue: TDoubleRect): THashValue;
    function CalcHashByBuffer(ABuffer: Pointer; ASize: Integer): THashValue;

    procedure UpdateHashByString(var AHash: THashValue; const AValue: string);
    procedure UpdateHashByAnsiString(var AHash: THashValue; const AValue: AnsiString);
    procedure UpdateHashByGUID(var AHash: THashValue; const AValue: TGUID);
    procedure UpdateHashByDouble(var AHash: THashValue; const AValue: Double);
    procedure UpdateHashByInteger(var AHash: THashValue; const AValue: Integer);
    procedure UpdateHashByPoint(var AHash: THashValue; const AValue: TPoint);
    procedure UpdateHashByDoublePoint(var AHash: THashValue; const AValue: TDoublePoint);
    procedure UpdateHashByRect(var AHash: THashValue; const AValue: TRect);
    procedure UpdateHashByDoubleRect(var AHash: THashValue; const AValue: TDoubleRect);
    procedure UpdateHashByBuffer(var AHash: THashValue; ABuffer: Pointer; ASize: Integer);
    procedure UpdateHashByHash(var AHash: THashValue; AValue: THashValue);
  public
    constructor Create(
      const AFunction: IHashFunctionImpl;
      const ACounterList: IInternalPerformanceCounterList
    );
  end;

implementation

const
  CSmallBufferSize = 32;

{ THashFunctionWithCounter }

constructor THashFunctionWithCounter.Create(
  const AFunction: IHashFunctionImpl;
  const ACounterList: IInternalPerformanceCounterList
);
begin
  Assert(Assigned(AFunction));
  Assert(Assigned(ACounterList));
  inherited Create;
  FFunction := AFunction;
  FCounterSmallString := ACounterList.CreateAndAddNewCounter('SmallString');
  FCounterBigString := ACounterList.CreateAndAddNewCounter('BigString');
  FCounterGUID := ACounterList.CreateAndAddNewCounter('GUID');
  FCounterDouble := ACounterList.CreateAndAddNewCounter('Double');
  FCounterInteger := ACounterList.CreateAndAddNewCounter('Integer');
  FCounterPoint := ACounterList.CreateAndAddNewCounter('Point');
  FCounterDoublePoint := ACounterList.CreateAndAddNewCounter('DoublePoint');
  FCounterRect := ACounterList.CreateAndAddNewCounter('Rect');
  FCounterDoubleRect := ACounterList.CreateAndAddNewCounter('DoubleRect');
  FCounterSmallBuffer := ACounterList.CreateAndAddNewCounter('SmallBuffer');
  FCounterBigBuffer := ACounterList.CreateAndAddNewCounter('BigBuffer');
  FCounterHash := ACounterList.CreateAndAddNewCounter('Hash');
end;

procedure THashFunctionWithCounter.UpdateHashByAnsiString(
  var AHash: THashValue;
  const AValue: AnsiString
);
var
  VSize: Integer;
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VSize := Length(AValue) * SizeOf(AValue[1]);
  if VSize > 0 then begin
    if VSize <= CSmallBufferSize then begin
      VCounter := FCounterSmallString;
    end else begin
      VCounter := FCounterBigString;
    end;
    VContext := VCounter.StartOperation;
    try
      AHash := FFunction.CalcHashWithSeed(@AValue[1], VSize, AHash);
    finally
      VCounter.FinishOperation(VContext);
    end;
  end;
end;

procedure THashFunctionWithCounter.UpdateHashByBuffer(
  var AHash: THashValue;
  ABuffer: Pointer;
  ASize: Integer
);
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  if ASize > 0 then begin
    if ASize <= CSmallBufferSize then begin
      VCounter := FCounterSmallBuffer;
    end else begin
      VCounter := FCounterBigBuffer;
    end;
    VContext := VCounter.StartOperation;
    try
      AHash := FFunction.CalcHashWithSeed(ABuffer, ASize, AHash);
    finally
      VCounter.FinishOperation(VContext);
    end;
  end;
end;

procedure THashFunctionWithCounter.UpdateHashByDouble(
  var AHash: THashValue;
  const AValue: Double
);
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := FCounterDouble;
  VContext := VCounter.StartOperation;
  try
    AHash := FFunction.CalcHashWithSeed(@AValue, SizeOf(AValue), AHash);
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

procedure THashFunctionWithCounter.UpdateHashByDoublePoint(
  var AHash: THashValue;
  const AValue: TDoublePoint
);
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := FCounterDoublePoint;
  VContext := VCounter.StartOperation;
  try
    AHash := FFunction.CalcHashWithSeed(@AValue, SizeOf(AValue), AHash);
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

procedure THashFunctionWithCounter.UpdateHashByDoubleRect(
  var AHash: THashValue;
  const AValue: TDoubleRect
);
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := FCounterDoubleRect;
  VContext := VCounter.StartOperation;
  try
    AHash := FFunction.CalcHashWithSeed(@AValue, SizeOf(AValue), AHash);
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

procedure THashFunctionWithCounter.UpdateHashByGUID(
  var AHash: THashValue;
  const AValue: TGUID
);
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := FCounterGUID;
  VContext := VCounter.StartOperation;
  try
    AHash := FFunction.CalcHashWithSeed(@AValue, SizeOf(AValue), AHash);
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

procedure THashFunctionWithCounter.UpdateHashByHash(
  var AHash: THashValue;
  AValue: THashValue
);
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := FCounterHash;
  VContext := VCounter.StartOperation;
  try
    AHash := FFunction.CalcHashWithSeed(@AValue, SizeOf(AValue), AHash);
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

procedure THashFunctionWithCounter.UpdateHashByInteger(
  var AHash: THashValue;
  const AValue: Integer
);
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := FCounterInteger;
  VContext := VCounter.StartOperation;
  try
    AHash := FFunction.CalcHashWithSeed(@AValue, SizeOf(AValue), AHash);
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

procedure THashFunctionWithCounter.UpdateHashByPoint(
  var AHash: THashValue;
  const AValue: TPoint
);
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := FCounterPoint;
  VContext := VCounter.StartOperation;
  try
    AHash := FFunction.CalcHashWithSeed(@AValue, SizeOf(AValue), AHash);
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

procedure THashFunctionWithCounter.UpdateHashByRect(
  var AHash: THashValue;
  const AValue: TRect
);
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := FCounterRect;
  VContext := VCounter.StartOperation;
  try
    AHash := FFunction.CalcHashWithSeed(@AValue, SizeOf(AValue), AHash);
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

procedure THashFunctionWithCounter.UpdateHashByString(
  var AHash: THashValue;
  const AValue: string
);
var
  VSize: Integer;
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VSize := Length(AValue) * SizeOf(AValue[1]);
  if VSize > 0 then begin
    if VSize <= CSmallBufferSize then begin
      VCounter := FCounterSmallString;
    end else begin
      VCounter := FCounterBigString;
    end;
    VContext := VCounter.StartOperation;
    try
      AHash := FFunction.CalcHashWithSeed(@AValue[1], VSize, AHash);
    finally
      VCounter.FinishOperation(VContext);
    end;
  end;
end;

function THashFunctionWithCounter.CalcHashByAnsiString(
  const AValue: AnsiString
): THashValue;
var
  VSize: Integer;
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VSize := Length(AValue) * SizeOf(AValue[1]);
  if VSize <= CSmallBufferSize then begin
    VCounter := FCounterSmallString;
  end else begin
    VCounter := FCounterBigString;
  end;
  VContext := VCounter.StartOperation;
  try
    Result := FFunction.CalcHash(@AValue[1], VSize);
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

function THashFunctionWithCounter.CalcHashByBuffer(
  ABuffer: Pointer;
  ASize: Integer
): THashValue;
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  if ASize <= CSmallBufferSize then begin
    VCounter := FCounterSmallBuffer;
  end else begin
    VCounter := FCounterBigBuffer;
  end;
  VContext := VCounter.StartOperation;
  try
    Result := FFunction.CalcHash(ABuffer, ASize);
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

function THashFunctionWithCounter.CalcHashByDouble(
  const AValue: Double
): THashValue;
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := FCounterDouble;
  VContext := VCounter.StartOperation;
  try
    Result := FFunction.CalcHash(@AValue, SizeOf(AValue));
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

function THashFunctionWithCounter.CalcHashByDoublePoint(
  const AValue: TDoublePoint
): THashValue;
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := FCounterDoublePoint;
  VContext := VCounter.StartOperation;
  try
    Result := FFunction.CalcHash(@AValue, SizeOf(AValue));
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

function THashFunctionWithCounter.CalcHashByDoubleRect(
  const AValue: TDoubleRect
): THashValue;
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := FCounterDoubleRect;
  VContext := VCounter.StartOperation;
  try
    Result := FFunction.CalcHash(@AValue, SizeOf(AValue));
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

function THashFunctionWithCounter.CalcHashByGUID(
  const AValue: TGUID
): THashValue;
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := FCounterGUID;
  VContext := VCounter.StartOperation;
  try
    Result := FFunction.CalcHash(@AValue, SizeOf(AValue));
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

function THashFunctionWithCounter.CalcHashByInteger(
  const AValue: Integer
): THashValue;
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := FCounterInteger;
  VContext := VCounter.StartOperation;
  try
    Result := FFunction.CalcHash(@AValue, SizeOf(AValue));
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

function THashFunctionWithCounter.CalcHashByPoint(
  const AValue: TPoint
): THashValue;
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := FCounterPoint;
  VContext := VCounter.StartOperation;
  try
    Result := FFunction.CalcHash(@AValue, SizeOf(AValue));
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

function THashFunctionWithCounter.CalcHashByRect(
  const AValue: TRect
): THashValue;
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := FCounterRect;
  VContext := VCounter.StartOperation;
  try
    Result := FFunction.CalcHash(@AValue, SizeOf(AValue));
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

function THashFunctionWithCounter.CalcHashByString(
  const AValue: string
): THashValue;
var
  VSize: Integer;
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VSize := Length(AValue) * SizeOf(AValue[1]);
  if VSize <= CSmallBufferSize then begin
    VCounter := FCounterSmallString;
  end else begin
    VCounter := FCounterBigString;
  end;
  VContext := VCounter.StartOperation;
  try
    Result := FFunction.CalcHash(@AValue[1], VSize);
  finally
    VCounter.FinishOperation(VContext);
  end;
end;

end.
