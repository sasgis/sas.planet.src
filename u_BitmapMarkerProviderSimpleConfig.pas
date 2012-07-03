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

unit u_BitmapMarkerProviderSimpleConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_BitmapMarkerProviderSimpleConfig,
  u_ConfigDataElementBase;

type
  TBitmapMarkerProviderSimpleConfig = class(TConfigDataElementWithStaticBase, IBitmapMarkerProviderSimpleConfig)
  private
    FDefault: IBitmapMarkerProviderSimpleConfigStatic;
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

    function GetStatic: IBitmapMarkerProviderSimpleConfigStatic;
  public
    constructor Create(const ADefault: IBitmapMarkerProviderSimpleConfigStatic);
  end;

implementation

uses
  u_ConfigProviderHelpers,
  u_BitmapMarkerProviderSimpleConfigStatic;

{ TBitmapMarkerProviderSimpleConfig }

constructor TBitmapMarkerProviderSimpleConfig.Create(
  const ADefault: IBitmapMarkerProviderSimpleConfigStatic
);
begin
  inherited Create;
  FDefault := ADefault;
  FMarkerSize := FDefault.MarkerSize;
  FMarkerColor := FDefault.MarkerColor;
  FBorderColor := FDefault.BorderColor;
end;

function TBitmapMarkerProviderSimpleConfig.CreateStatic: IInterface;
var
  VStatic: IBitmapMarkerProviderSimpleConfigStatic;
begin
  VStatic :=
    TBitmapMarkerProviderSimpleConfigStatic.Create(
      FMarkerSize,
      FMarkerColor,
      FBorderColor
    );
  Result := VStatic;
end;

procedure TBitmapMarkerProviderSimpleConfig.DoReadConfig(
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

procedure TBitmapMarkerProviderSimpleConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteInteger('Size', FMarkerSize);
  WriteColor32(AConfigData, 'FillColor', FMarkerColor);
  WriteColor32(AConfigData, 'BorderColor', FBorderColor);
end;

function TBitmapMarkerProviderSimpleConfig.GetBorderColor: TColor32;
begin
  LockRead;
  try
    Result := FBorderColor;
  finally
    UnlockRead;
  end;
end;

function TBitmapMarkerProviderSimpleConfig.GetMarkerColor: TColor32;
begin
  LockRead;
  try
    Result := FMarkerColor;
  finally
    UnlockRead;
  end;
end;

function TBitmapMarkerProviderSimpleConfig.GetMarkerSize: Integer;
begin
  LockRead;
  try
    Result := FMarkerSize;
  finally
    UnlockRead;
  end;
end;

function TBitmapMarkerProviderSimpleConfig.GetStatic: IBitmapMarkerProviderSimpleConfigStatic;
begin
  Result := IBitmapMarkerProviderSimpleConfigStatic(GetStaticInternal);
end;

procedure TBitmapMarkerProviderSimpleConfig.SetBorderColor(AValue: TColor32);
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

procedure TBitmapMarkerProviderSimpleConfig.SetMarkerColor(AValue: TColor32);
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

procedure TBitmapMarkerProviderSimpleConfig.SetMarkerSize(AValue: Integer);
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
