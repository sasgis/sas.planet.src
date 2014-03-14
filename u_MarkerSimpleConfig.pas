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

unit u_MarkerSimpleConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarkerSimpleConfig,
  u_ConfigDataElementBase;

type
  TMarkerSimpleConfig = class(TConfigDataElementWithStaticBase, IMarkerSimpleConfig)
  private
    FDefault: IMarkerSimpleConfigStatic;
    FMarkerSize: Integer;
    FMarkerColor: TColor32;
    FBorderColor: TColor32;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetMarkerSize: Integer;
    procedure SetMarkerSize(AValue: Integer);

    function GetMarkerColor: TColor32;
    procedure SetMarkerColor(AValue: TColor32);

    function GetBorderColor: TColor32;
    procedure SetBorderColor(AValue: TColor32);

    function GetStatic: IMarkerSimpleConfigStatic;
  public
    constructor Create(const ADefault: IMarkerSimpleConfigStatic);
  end;

implementation

uses
  u_ConfigProviderHelpers,
  u_MarkerSimpleConfigStatic;

{ TMarkerSimpleConfig }

constructor TMarkerSimpleConfig.Create(
  const ADefault: IMarkerSimpleConfigStatic
);
begin
  inherited Create;
  FDefault := ADefault;
  FMarkerSize := FDefault.MarkerSize;
  FMarkerColor := FDefault.MarkerColor;
  FBorderColor := FDefault.BorderColor;
end;

function TMarkerSimpleConfig.CreateStatic: IInterface;
var
  VStatic: IMarkerSimpleConfigStatic;
begin
  VStatic :=
    TMarkerSimpleConfigStatic.Create(
      FMarkerSize,
      FMarkerColor,
      FBorderColor
    );
  Result := VStatic;
end;

procedure TMarkerSimpleConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    FMarkerSize := AConfigData.ReadInteger('Size', FMarkerSize);
    FMarkerColor := ReadColor32(AConfigData, 'FillColor', FMarkerColor);
    FBorderColor := ReadColor32(AConfigData, 'BorderColor', FBorderColor);
    SetChanged;
  end;
end;

procedure TMarkerSimpleConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteInteger('Size', FMarkerSize);
  WriteColor32(AConfigData, 'FillColor', FMarkerColor);
  WriteColor32(AConfigData, 'BorderColor', FBorderColor);
end;

function TMarkerSimpleConfig.GetBorderColor: TColor32;
begin
  LockRead;
  try
    Result := FBorderColor;
  finally
    UnlockRead;
  end;
end;

function TMarkerSimpleConfig.GetMarkerColor: TColor32;
begin
  LockRead;
  try
    Result := FMarkerColor;
  finally
    UnlockRead;
  end;
end;

function TMarkerSimpleConfig.GetMarkerSize: Integer;
begin
  LockRead;
  try
    Result := FMarkerSize;
  finally
    UnlockRead;
  end;
end;

function TMarkerSimpleConfig.GetStatic: IMarkerSimpleConfigStatic;
begin
  Result := IMarkerSimpleConfigStatic(GetStaticInternal);
end;

procedure TMarkerSimpleConfig.SetBorderColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FBorderColor <> AValue then begin
      FBorderColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMarkerSimpleConfig.SetMarkerColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FMarkerColor <> AValue then begin
      FMarkerColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMarkerSimpleConfig.SetMarkerSize(AValue: Integer);
begin
  LockWrite;
  try
    if FMarkerSize <> AValue then begin
      FMarkerSize := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.




