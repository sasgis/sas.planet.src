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

unit u_KmlLayerConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ThreadConfig,
  i_KmlLayerConfig,
  u_ConfigDataElementComplexBase;

type
  TKmlLayerConfig = class(TConfigDataElementComplexBase, IKmlLayerConfig)
  private
    FMainColor: TColor32;
    FPointColor: TColor32;
    FShadowColor: TColor32;
    FThreadConfig: IThreadConfig;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetMainColor: TColor32;
    procedure SetMainColor(AValue: TColor32);

    function GetPointColor: TColor32;
    procedure SetPointColor(AValue: TColor32);

    function GetShadowColor: TColor32;
    procedure SetShadowColor(AValue: TColor32);

    function GetThreadConfig: IThreadConfig;
  public
    constructor Create;
  end;
implementation

uses
  Classes,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_ThreadConfig,
  u_ConfigProviderHelpers;

{ TKmlLayerConfig }

constructor TKmlLayerConfig.Create;
begin
  inherited;
  FMainColor := clWhite32;
  FShadowColor := clBlack32;
  FPointColor := SetAlpha(clWhite32, 170);

  FThreadConfig := TThreadConfig.Create(tpLowest);
  Add(FThreadConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

procedure TKmlLayerConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FMainColor := ReadColor32(AConfigData, 'MainColor', FMainColor);
    FPointColor := ReadColor32(AConfigData, 'PointColor', FPointColor);
    FShadowColor := ReadColor32(AConfigData, 'ShadowColor', FShadowColor);
    SetChanged;
  end;
end;

procedure TKmlLayerConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  WriteColor32(AConfigData, 'MainColor', FMainColor);
  WriteColor32(AConfigData, 'PointColor', FPointColor);
  WriteColor32(AConfigData, 'ShadowColor', FShadowColor);
end;

function TKmlLayerConfig.GetMainColor: TColor32;
begin
  LockRead;
  try
    Result := FMainColor;
  finally
    UnlockRead;
  end;
end;

function TKmlLayerConfig.GetPointColor: TColor32;
begin
  LockRead;
  try
    Result := FPointColor;
  finally
    UnlockRead;
  end;
end;

function TKmlLayerConfig.GetShadowColor: TColor32;
begin
  LockRead;
  try
    Result := FShadowColor;
  finally
    UnlockRead;
  end;
end;

function TKmlLayerConfig.GetThreadConfig: IThreadConfig;
begin
  Result := FThreadConfig;
end;

procedure TKmlLayerConfig.SetMainColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FMainColor <> AValue then begin
      FMainColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TKmlLayerConfig.SetPointColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FPointColor <> AValue then begin
      FPointColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TKmlLayerConfig.SetShadowColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FShadowColor <> AValue then begin
      FShadowColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
