{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_BitmapPostProcessingConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_Bitmap32StaticFactory,
  i_BitmapPostProcessingConfig,
  u_ConfigDataElementBase;

type
  TBitmapPostProcessingConfig = class(TConfigDataElementWithStaticBase, IBitmapPostProcessingConfig)
  private
    FBitmapFactory: IBitmap32StaticFactory;
    FInvertColor: boolean;
    FGammaN: Integer;
    FContrastN: Integer;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetInvertColor: boolean;
    procedure SetInvertColor(const AValue: boolean);

    function GetGammaN: Integer;
    procedure SetGammaN(const AValue: Integer);

    function GetContrastN: Integer;
    procedure SetContrastN(const AValue: Integer);

    function GetStatic: IBitmapPostProcessing;
  public
    constructor Create(const ABitmapFactory: IBitmap32StaticFactory);
  end;

implementation

uses
  u_BitmapPostProcessingConfigStatic;

function BLimit(B: Integer): Byte;
begin
  if B < 0 then begin
    Result := 0;
  end else if B > 255 then begin
    Result := 255;
  end else begin
    Result := B;
  end;
end;

function Power(Base, Exponent: Extended): Extended;
begin
  Result := Exp(Exponent * Ln(Base));
end;

function GetContrastTable(
  const AContrastN: Integer
): TComponentTable;
var
  mr, i: Integer;
  vd: Double;
begin
  mr := 128;
  if AContrastN > 0 then begin
    vd := 1 + (AContrastN / 100);
  end else begin
    vd := 1 - (Sqrt(-AContrastN / 1000));
  end;
  for i := 0 to 255 do begin
    Result[i] := BLimit(mr + Trunc((i - mr) * vd));
  end;
end;

function GetGammaTable(const AGammaN: Integer): TComponentTable;
var
  X: integer;
  L: Double;
begin
  if AGammaN < 50 then begin
    L := 1 / ((AGammaN * 2) / 100);
  end else begin
    L := 1 / ((AGammaN - 40) / 10);
  end;
  Result[0] := 0;
  for X := 1 to 255 do begin
    Result[X] := round(255 * Power(X / 255, L));
  end;
end;

function GetInvertTable: TComponentTable;
var
  i: Integer;
begin
  for i := 0 to 255 do begin
    Result[i] := i xor $FF;
  end;
end;

function GetEqualTable: TComponentTable;
var
  i: Integer;
begin
  for i := 0 to 255 do begin
    Result[i] := i;
  end;
end;

function CombineTables(const ATable1, ATable2: TComponentTable): TComponentTable;
var
  i: Integer;
begin
  for i := 0 to 255 do begin
    Result[i] := ATable1[ATable2[i]];
  end;
end;

{ TBitmapPostProcessingConfig }

constructor TBitmapPostProcessingConfig.Create(const ABitmapFactory: IBitmap32StaticFactory);
begin
  inherited Create;
  FBitmapFactory := ABitmapFactory;
  FInvertColor := False;
  FContrastN := 0;
  FGammaN := 50;
end;

function TBitmapPostProcessingConfig.CreateStatic: IInterface;
var
  VStatic: IBitmapPostProcessing;
  VTable: TComponentTable;
begin
  if (FContrastN = 0) and (not FInvertColor) and (FGammaN = 50) then begin
    VStatic := TBitmapPostProcessingSimple.Create;
  end else begin
    VTable := GetEqualTable;
    if FContrastN <> 0 then begin
      VTable := CombineTables(GetContrastTable(FContrastN), VTable);
    end;
    if FInvertColor then begin
      VTable := CombineTables(GetInvertTable, VTable);
    end;
    if FGammaN <> 50 then begin
      VTable := CombineTables(GetGammaTable(FGammaN), VTable);
    end;
    VStatic := TBitmapPostProcessingByTable.Create(FBitmapFactory, VTable);
  end;
  Result := VStatic;
end;

procedure TBitmapPostProcessingConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    FInvertColor := AConfigData.ReadBool('InvertColor', FInvertColor);
    FGammaN := AConfigData.ReadInteger('Gamma', FGammaN);
    FContrastN := AConfigData.ReadInteger('Contrast', FContrastN);
    SetChanged;
  end;
end;

procedure TBitmapPostProcessingConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('InvertColor', FInvertColor);
  AConfigData.WriteInteger('Gamma', FGammaN);
  AConfigData.WriteInteger('Contrast', FContrastN);
end;

function TBitmapPostProcessingConfig.GetContrastN: Integer;
begin
  LockRead;
  try
    Result := FContrastN;
  finally
    UnlockRead;
  end;
end;

function TBitmapPostProcessingConfig.GetGammaN: Integer;
begin
  LockRead;
  try
    Result := FGammaN;
  finally
    UnlockRead;
  end;
end;

function TBitmapPostProcessingConfig.GetInvertColor: boolean;
begin
  LockRead;
  try
    Result := FInvertColor;
  finally
    UnlockRead;
  end;
end;

function TBitmapPostProcessingConfig.GetStatic: IBitmapPostProcessing;
begin
  Result := IBitmapPostProcessing(GetStaticInternal);
end;

procedure TBitmapPostProcessingConfig.SetContrastN(const AValue: Integer);
begin
  LockWrite;
  try
    if FContrastN <> AValue then begin
      FContrastN := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapPostProcessingConfig.SetGammaN(const AValue: Integer);
begin
  LockWrite;
  try
    if FGammaN <> AValue then begin
      FGammaN := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapPostProcessingConfig.SetInvertColor(const AValue: boolean);
begin
  LockWrite;
  try
    if FInvertColor <> AValue then begin
      FInvertColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
