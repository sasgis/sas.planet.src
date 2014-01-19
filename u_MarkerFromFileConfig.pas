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

unit u_MarkerFromFileConfig;

interface

uses
  t_GeoTypes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarkerFromFileConfig,
  u_ConfigDataElementBase;

type
  TMarkerFromFileConfig = class(TConfigDataElementWithStaticBase, IMarkerFromFileConfig)
  private
    FDefault: IMarkerFromFileConfigStatic;
    FFileName: string;
    FAnchorType: TAnchorType;
    FFixedPoint: TDoublePoint;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetFileName: string;
    procedure SetFileName(const AValue: string);

    function GetAnchorType: TAnchorType;
    procedure SetAnchorType(const AValue: TAnchorType);

    function GetFixedPoint: TDoublePoint;
    procedure SetFixedPoint(const AValue: TDoublePoint);

    function GetStatic: IMarkerFromFileConfigStatic;
  public
    constructor Create(const ADefault: IMarkerFromFileConfigStatic);
  end;

implementation

uses
  u_MarkerFromFileConfigStatic,
  u_GeoFunc;

{ TMarkerFromFileConfig }

constructor TMarkerFromFileConfig.Create(
  const ADefault: IMarkerFromFileConfigStatic);
begin
  inherited Create;
  FDefault := ADefault;
  FFileName := FDefault.FileName;
  FAnchorType := FDefault.AnchorType;
  FFixedPoint := FDefault.FixedPoint;
end;

function TMarkerFromFileConfig.CreateStatic: IInterface;
var
  VStatic: IMarkerFromFileConfigStatic;
begin
  VStatic :=
    TMarkerFromFileConfigStatic.Create(
      FFileName,
      FAnchorType,
      FFixedPoint
    );
  Result := VStatic;
end;

procedure TMarkerFromFileConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FFileName := AConfigData.ReadString('FileName', FFileName);
    FAnchorType := TAnchorType(AConfigData.ReadInteger('AnchorType', Integer(FAnchorType)));
    FFixedPoint.X := AConfigData.ReadFloat('AnchorPoint_X', FFixedPoint.X);
    FFixedPoint.Y := AConfigData.ReadFloat('AnchorPoint_Y', FFixedPoint.Y);
    SetChanged;
  end;
end;

procedure TMarkerFromFileConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteString('FileName', FFileName);
  AConfigData.WriteInteger('AnchorType', Integer(FAnchorType));
  AConfigData.WriteFloat('AnchorPoint_X', FFixedPoint.X);
  AConfigData.WriteFloat('AnchorPoint_Y', FFixedPoint.Y);
end;

function TMarkerFromFileConfig.GetAnchorType: TAnchorType;
begin
  LockRead;
  try
    Result := FAnchorType;
  finally
    UnlockRead;
  end;
end;

function TMarkerFromFileConfig.GetFileName: string;
begin
  LockRead;
  try
    Result := FFileName;
  finally
    UnlockRead;
  end;
end;

function TMarkerFromFileConfig.GetFixedPoint: TDoublePoint;
begin
  LockRead;
  try
    Result := FFixedPoint;
  finally
    UnlockRead;
  end;
end;

function TMarkerFromFileConfig.GetStatic: IMarkerFromFileConfigStatic;
begin
  Result := IMarkerFromFileConfigStatic(GetStaticInternal);
end;

procedure TMarkerFromFileConfig.SetAnchorType(const AValue: TAnchorType);
begin
  LockWrite;
  try
    if FAnchorType <> AValue then begin
      FAnchorType := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMarkerFromFileConfig.SetFileName(const AValue: string);
begin
  LockWrite;
  try
    if FFileName <> AValue then begin
      FFileName := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMarkerFromFileConfig.SetFixedPoint(const AValue: TDoublePoint);
begin
  LockWrite;
  try
    if not DoublePointsEqual(FFixedPoint, AValue) then begin
      FFixedPoint := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
