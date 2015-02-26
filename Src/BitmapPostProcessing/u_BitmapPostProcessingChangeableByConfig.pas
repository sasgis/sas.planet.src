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

unit u_BitmapPostProcessingChangeableByConfig;

interface

uses
  SysUtils,
  i_BitmapPostProcessing,
  i_BitmapPostProcessingConfig,
  i_Bitmap32BufferFactory,
  i_Listener,
  u_ChangeableBase;

type
  TBitmapPostProcessingChangeableByConfig = class(TChangeableWithSimpleLockBase, IBitmapPostProcessingChangeable)
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FConfig: IBitmapPostProcessingConfig;
    FConfigChangeListener: IListener;
    FPrevConfig: IBitmapPostProcessingConfigStatic;
    FStatic: IBitmapPostProcessing;

    procedure OnConfigChange;
    function CreateStatic(const AConfig: IBitmapPostProcessingConfigStatic): IBitmapPostProcessing;
  private
    function GetStatic: IBitmapPostProcessing;
  public
    constructor Create(
      const AConfig: IBitmapPostProcessingConfig;
      const ABitmap32StaticFactory: IBitmap32StaticFactory
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_BitmapPostProcessing,
  u_ListenerByEvent;

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

{ TBitmapPostProcessingChangeableByConfig }

constructor TBitmapPostProcessingChangeableByConfig.Create(
  const AConfig: IBitmapPostProcessingConfig;
  const ABitmap32StaticFactory: IBitmap32StaticFactory
);
begin
  Assert(Assigned(AConfig));
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create;
  FConfig := AConfig;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfig.ChangeNotifier.Add(FConfigChangeListener);
  OnConfigChange;
end;

function TBitmapPostProcessingChangeableByConfig.CreateStatic(
  const AConfig: IBitmapPostProcessingConfigStatic
): IBitmapPostProcessing;
var
  VStatic: IBitmapPostProcessing;
  VTable: TComponentTable;
begin
  if (AConfig.ContrastN = 0) and (not AConfig.InvertColor) and (AConfig.GammaN = 50) then begin
    VStatic := TBitmapPostProcessingSimple.Create;
  end else begin
    VTable := GetEqualTable;
    if AConfig.ContrastN <> 0 then begin
      VTable := CombineTables(GetContrastTable(AConfig.ContrastN), VTable);
    end;
    if AConfig.InvertColor then begin
      VTable := CombineTables(GetInvertTable, VTable);
    end;
    if AConfig.GammaN <> 50 then begin
      VTable := CombineTables(GetGammaTable(AConfig.GammaN), VTable);
    end;
    VStatic :=
      TBitmapPostProcessingByTable.Create(
        FBitmap32StaticFactory,
        VTable
      );
  end;
  Result := VStatic;
end;

destructor TBitmapPostProcessingChangeableByConfig.Destroy;
begin
  if Assigned(FConfig) and Assigned(FConfigChangeListener) then begin
    FConfig.ChangeNotifier.Remove(FConfigChangeListener);
    FConfigChangeListener := nil;
  end;
  inherited;
end;

function TBitmapPostProcessingChangeableByConfig.GetStatic: IBitmapPostProcessing;
begin
  CS.BeginRead;
  try
    Result := FStatic;
  finally
    CS.EndRead;
  end;
end;

procedure TBitmapPostProcessingChangeableByConfig.OnConfigChange;
var
  VNeedNotify: Boolean;
  VConfig: IBitmapPostProcessingConfigStatic;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    VConfig := FConfig.GetStatic;
    if VConfig <> FPrevConfig then begin
      FPrevConfig := VConfig;
      FStatic := CreateStatic(VConfig);
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

end.
