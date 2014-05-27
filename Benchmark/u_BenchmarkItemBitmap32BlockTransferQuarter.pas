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

unit u_BenchmarkItemBitmap32BlockTransferQuarter;

interface

uses
  GR32,
  u_BenchmarkItemBase;

type
  TBenchmarkItemBitmap32BlockTransferQuarter = class(TBenchmarkItemBase)
  private
    FSource: TCustomBitmap32;
    FDest: TCustomBitmap32;
    FDelta: Integer;
    FBitmapSize: Integer;
    FCombineOp: TDrawMode;
    FCombineMode: TCombineMode;
    FMasterAlpha: Cardinal;
  protected
    procedure SetUp; override;
    function RunOneStep: Integer; override;
    procedure TearDown; override;
  public
    constructor Create(
      const ABitmapSize: Integer;
      const ACombineOp: TDrawMode;
      const ACombineMode: TCombineMode;
      const AMasterAlpha: Cardinal
    );
  end;

implementation

uses
  SysUtils,
  GR32_Resamplers;

{ TBenchmarkItemBitmap32BlockTransferQuarter }

constructor TBenchmarkItemBitmap32BlockTransferQuarter.Create(
  const ABitmapSize: Integer;
  const ACombineOp: TDrawMode;
  const ACombineMode: TCombineMode;
  const AMasterAlpha: Cardinal
);
var
  VName: string;
  VDrawMode: string;
  VCombineMode: string;
begin
  Assert(ABitmapSize > 0);
  case ACombineOp of
    dmOpaque: VDrawMode := 'Opaque';
    dmBlend: VDrawMode := 'Blend';
    dmCustom: VDrawMode := 'Custom';
    dmTransparent: VDrawMode := 'Transparent';
  end;
  case ACombineMode of
    cmBlend: VCombineMode := 'Blend';
    cmMerge: VCombineMode := 'Merge';
  end;
  VName := Format('BlockTransferQuarter DrawMode=%s CombineMode=%s MasterAlpha=%d', [VDrawMode, VCombineMode, AMasterAlpha]);
  inherited Create(True, VName, ABitmapSize*ABitmapSize);
  FBitmapSize := ABitmapSize;
  FCombineOp := ACombineOp;
  FCombineMode := ACombineMode;
  FMasterAlpha := AMasterAlpha;
end;

function TBenchmarkItemBitmap32BlockTransferQuarter.RunOneStep: Integer;
begin
  GR32_Resamplers.BlockTransfer(
    FDest,
    FDelta,
    FDelta,
    FDest.ClipRect,
    FSource.Bits,
    FSource.Width,
    FSource.Height,
    FSource.ClipRect,
    FCombineOp,
    FCombineMode,
    FMasterAlpha,
    clBlack32
  );
  Inc(FDelta);
  if FDelta >= FBitmapSize then begin
    FDelta := 0;
  end;
  Result := FDest.Bits[0];
end;

procedure TBenchmarkItemBitmap32BlockTransferQuarter.SetUp;
var
  VColor: TColor32;
  i: Integer;
begin
  inherited;
  VColor := SetAlpha(clGray32, 128);

  FSource := TCustomBitmap32.Create;
  FSource.SetSize(FBitmapSize, FBitmapSize);
  for i := 0 to FBitmapSize*FBitmapSize - 1 do begin
    FSource.Bits[i] := VColor;
    Inc(VColor);
  end;

  FDest := TCustomBitmap32.Create;
  FDest.SetSize(FBitmapSize*2, FBitmapSize*2);
  for i := 0 to FBitmapSize*2*FBitmapSize*2 - 1 do begin
    FDest.Bits[i] := VColor;
    Inc(VColor);
  end;
end;

procedure TBenchmarkItemBitmap32BlockTransferQuarter.TearDown;
begin
  inherited;
  FreeAndNil(FSource);
  FreeAndNil(FDest);
end;

end.
