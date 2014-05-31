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

unit u_BenchmarkItemHashFunction;

interface

uses
  i_HashFunctionImpl,
  u_BenchmarkItemBase;

type
  TBenchmarkItemHashFunction = class(TBenchmarkItemBase)
  private
    FHashFunction: IHashFunctionImpl;
    FInputSize: Integer;
    FSourceData: array of Byte;
  protected
    procedure SetUp; override;
    function RunOneStep: Integer; override;
  public
    constructor Create(
      const AHashName: string;
      const AInputSize: Integer;
      const AHashFunction: IHashFunctionImpl
    );
  end;

implementation

uses
  t_Hash,
  SysUtils;

const CRepeatCount = 1000;

{ TBenchmarkItemHashFunction }

constructor TBenchmarkItemHashFunction.Create(
  const AHashName: string;
  const AInputSize: Integer;
  const AHashFunction: IHashFunctionImpl
);
begin
  Assert(AInputSize > 0);
  inherited Create(
    Assigned(AHashFunction),
    'Hash ' + AHashName + ' x' + IntToStr(AInputSize),
    CRepeatCount*AInputSize
  );
  FInputSize := AInputSize;
  FHashFunction := AHashFunction;
end;

function TBenchmarkItemHashFunction.RunOneStep: Integer;
var
  i: Integer;
  VHash: THashValue;
begin
  VHash := 1;
  for i := 0 to CRepeatCount - 1 do begin
    VHash := FHashFunction.CalcHashWithSeed(@FSourceData[0], FInputSize, VHash);
  end;
  Result := VHash;
end;

procedure TBenchmarkItemHashFunction.SetUp;
var
  i: Integer;
begin
  inherited;
  SetLength(FSourceData, FInputSize);
  for i := 0 to FInputSize - 1 do begin
    FSourceData[i] := i;
  end;
end;

end.
