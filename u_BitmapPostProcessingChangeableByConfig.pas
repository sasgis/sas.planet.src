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
  i_BitmapPostProcessing,
  i_BitmapPostProcessingConfig,
  i_Bitmap32StaticFactory,
  i_Listener,
  u_ConfigDataElementBase;

type
  TBitmapPostProcessingChangeableByConfig = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IBitmapPostProcessingChangeable)
  private
    FBitmapFactory: IBitmap32StaticFactory;
    FConfig: IBitmapPostProcessingConfig;
    FConfigChangeListener: IListener;
    procedure  OnConfigChange;
  protected
    function CreateStatic: IInterface; override;
  private
    function GetStatic: IBitmapPostProcessing;
  public
    constructor Create(
      const AConfig: IBitmapPostProcessingConfig;
      const ABitmapFactory: IBitmap32StaticFactory
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
  const ABitmapFactory: IBitmap32StaticFactory
);
begin
  Assert(AConfig <> nil);
  Assert(ABitmapFactory <> nil);
  inherited Create;
  FConfig := AConfig;
  FBitmapFactory := ABitmapFactory;
  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfig.ChangeNotifier.Add(FConfigChangeListener);
end;

function TBitmapPostProcessingChangeableByConfig.CreateStatic: IInterface;
var
  VStatic: IBitmapPostProcessing;
  VTable: TComponentTable;
  VConfig: IBitmapPostProcessingConfigStatic;
begin
  VConfig := FConfig.GetStatic;
  if (VConfig.ContrastN = 0) and (not VConfig.InvertColor) and (VConfig.GammaN = 50) then begin
    VStatic := TBitmapPostProcessingSimple.Create;
  end else begin
    VTable := GetEqualTable;
    if VConfig.ContrastN <> 0 then begin
      VTable := CombineTables(GetContrastTable(VConfig.ContrastN), VTable);
    end;
    if VConfig.InvertColor then begin
      VTable := CombineTables(GetInvertTable, VTable);
    end;
    if VConfig.GammaN <> 50 then begin
      VTable := CombineTables(GetGammaTable(VConfig.GammaN), VTable);
    end;
    VStatic := TBitmapPostProcessingByTable.Create(FBitmapFactory, VTable);
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
  Result := IBitmapPostProcessing(GetStaticInternal);
end;

procedure TBitmapPostProcessingChangeableByConfig.OnConfigChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
