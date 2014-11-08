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

unit u_MarkerRingsConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarkerRingsConfig,
  u_ConfigDataElementBase;

type
  TMarkerRingsConfig = class(TConfigDataElementWithStaticBase, IMarkerRingsConfig)
  private
    FCount: Integer;
    FStepDistance: Double;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetCount: Integer;
    procedure SetCount(AValue: Integer);

    function GetStepDistance: Double;
    procedure SetStepDistance(AValue: Double);

    function GetStatic: IMarkerRingsConfigStatic;
  public
    constructor Create;
  end;

implementation

uses
  u_MarkerRingsConfigStatic;

{ TMarkerRingsConfig }

constructor TMarkerRingsConfig.Create;
begin
  inherited Create;
  FCount := 0;
  FStepDistance := 1000;
end;

function TMarkerRingsConfig.CreateStatic: IInterface;
var
  VStatic: IMarkerRingsConfigStatic;
begin
  VStatic :=
    TMarkerRingsConfigStatic.Create(
      FCount,
      FStepDistance
    );
  Result := VStatic;
end;

procedure TMarkerRingsConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetCount(AConfigData.ReadInteger('Count', FCount));
    SetStepDistance(AConfigData.ReadFloat('StepDistance', FStepDistance));
  end;
end;

procedure TMarkerRingsConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('Count', FCount);
  AConfigData.WriteFloat('StepDistance', FStepDistance);
end;

function TMarkerRingsConfig.GetCount: Integer;
begin
  LockRead;
  try
    Result := FCount;
  finally
    UnlockRead;
  end;
end;

function TMarkerRingsConfig.GetStepDistance: Double;
begin
  LockRead;
  try
    Result := FStepDistance;
  finally
    UnlockRead;
  end;
end;

function TMarkerRingsConfig.GetStatic: IMarkerRingsConfigStatic;
begin
  Result := IMarkerRingsConfigStatic(GetStaticInternal);
end;

procedure TMarkerRingsConfig.SetCount(AValue: Integer);
var
  VValue: Integer;
begin
  VValue := AValue;
  if VValue < 0 then begin
    VValue := 0;
  end;
  if VValue > 20 then begin
    VValue := 20;
  end;

  LockWrite;
  try
    if FCount <> VValue then begin
      FCount := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMarkerRingsConfig.SetStepDistance(AValue: Double);
var
  VValue: Double;
begin
  VValue := AValue;
  if VValue < 1 then begin
    VValue := 1;
  end;
  if VValue > 1000000 then begin
    VValue := 1000000;
  end;

  LockWrite;
  try
    if FStepDistance <> VValue then begin
      FStepDistance := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
