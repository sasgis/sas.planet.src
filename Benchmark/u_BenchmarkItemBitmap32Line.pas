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

unit u_BenchmarkItemBitmap32Line;

interface

uses
  GR32,
  u_BenchmarkItemBase;

type
  TBenchmarkItemBitmap32Line = class(TBenchmarkItemBase)
  private
    FDest: TCustomBitmap32;
    FBitmapSize: Integer;
    FAntialiasing: Boolean;
    FUseSafe: Boolean;
    FUseTransparent: Boolean;
    FCombineMode: TCombineMode;
    FColor: TColor32;
  protected
    procedure SetUp; override;
    function RunOneStep: Integer; override;
    procedure TearDown; override;
  public
    constructor Create(
      const ABitmapSize: Integer;
      const AUseSafe: Boolean;
      const AAntialiasing: Boolean;
      const AUseTransparent: Boolean;
      const ACombineMode: TCombineMode
    );
  end;

implementation

uses
  SysUtils;

{ TBenchmarkItemBitmap32Line }

constructor TBenchmarkItemBitmap32Line.Create(
  const ABitmapSize: Integer;
  const AUseSafe: Boolean;
  const AAntialiasing: Boolean;
  const AUseTransparent: Boolean;
  const ACombineMode: TCombineMode
);
var
  VName: string;
  VCombineMode: string;
  VSafe: string;
  VAntialiasing: string;
  VTransparent: string;
  VTotalLineLen: Double;
  i: Integer;
begin
  Assert(ABitmapSize > 0);
  if AUseSafe then begin
    VSafe := 'Safe';
  end else begin
    VSafe := 'Unsafe';
  end;
  if AAntialiasing then begin
    VAntialiasing := 'Antialiasing';
  end else begin
    VAntialiasing := '';
  end;
  if AUseTransparent then begin
    case ACombineMode of
      cmBlend: VCombineMode := 'Blend';
      cmMerge: VCombineMode := 'Merge';
    end;
    VTransparent := 'Transparent '+ VCombineMode;
  end else begin
    VTransparent := 'Solid';
  end;
  VName := Format('Line %s %s %s', [VSafe, VAntialiasing, VTransparent]);
  VTotalLineLen := 0;
  for i := 1 to ABitmapSize - 2 do begin
    VTotalLineLen := VTotalLineLen + Sqrt(Sqr(ABitmapSize - i) + sqr(i))
  end;

  inherited Create(True, VName, Trunc(VTotalLineLen));
  FBitmapSize := ABitmapSize;
  FUseSafe := AUseSafe;
  FAntialiasing := AAntialiasing;
  FUseTransparent := AUseTransparent;
  FCombineMode := ACombineMode;
  if AUseTransparent then begin
    FColor := SetAlpha(clGray32, 128);
  end else begin
    FColor := clLightGray32;
  end;
end;

function TBenchmarkItemBitmap32Line.RunOneStep: Integer;
var
  i: Integer;
begin
  for i := 1 to FBitmapSize - 2 do begin
    if FAntialiasing then begin
      if FUseSafe then begin
        if FUseTransparent then begin
          FDest.LineFS(i, 0, FBitmapSize - 1, i,  FColor);
        end else begin
          FDest.LineAS(i, 0, FBitmapSize - 1, i,  FColor);
        end;
      end else begin
        if FUseTransparent then begin
          FDest.LineF(i, 0, FBitmapSize - 1, i,  FColor);
        end else begin
          FDest.LineA(i, 0, FBitmapSize - 1, i,  FColor);
        end;
      end;
    end else begin
      if FUseSafe then begin
        if FUseTransparent then begin
          FDest.LineTS(i, 0, FBitmapSize - 1, i,  FColor);
        end else begin
          FDest.LineS(i, 0, FBitmapSize - 1, i,  FColor);
        end;
      end else begin
        if FUseTransparent then begin
          FDest.LineT(i, 0, FBitmapSize - 1, i,  FColor);
        end else begin
          FDest.Line(i, 0, FBitmapSize - 1, i,  FColor);
        end;
      end;
    end;
  end;
  Result := FDest.Bits[0];
end;

procedure TBenchmarkItemBitmap32Line.SetUp;
var
  VColor: TColor32;
  i: Integer;
begin
  inherited;
  VColor := SetAlpha(clGray32, 128);

  FDest := TCustomBitmap32.Create;
  FDest.SetSize(FBitmapSize, FBitmapSize);
  FDest.CombineMode := FCombineMode;
  for i := 0 to FBitmapSize*FBitmapSize - 1 do begin
    FDest.Bits[i] := VColor;
    Inc(VColor);
  end;
end;

procedure TBenchmarkItemBitmap32Line.TearDown;
begin
  inherited;
  FreeAndNil(FDest);
end;

end.
