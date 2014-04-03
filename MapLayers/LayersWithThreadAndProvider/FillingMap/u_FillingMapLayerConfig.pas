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

unit u_FillingMapLayerConfig;

interface

uses
  DateUtils,
  SysUtils,
  t_Bitmap32,
  t_FillingMapModes,
  i_ThreadConfig,
  i_MapTypes,
  i_MapTypeSet,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_FillingMapLayerConfig,
  u_ConfigDataElementComplexBase;

type
  TFillingMapLayerConfig = class(TConfigDataElementComplexWithStaticBase, IFillingMapLayerConfig)
  private
    FVisible: Boolean;
    FUseRelativeZoom: Boolean;
    FZoom: Byte;
    FNoTileColor: TColor32;
    FShowTNE: Boolean;
    FTNEColor: TColor32;
    FSourceMap: IFillingMapMapsConfig;
    FFillMode: TFillMode;
    FFilterMode: Boolean;
    FFillFirstDay: TDateTime;
    FFillLastDay: TDateTime;
    FThreadConfig: IThreadConfig;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);

    function GetUseRelativeZoom: Boolean;
    procedure SetUseRelativeZoom(const AValue: Boolean);

    function GetZoom: Byte;
    procedure SetZoom(const AValue: Byte);

    function GetNoTileColor: TColor32;
    procedure SetNoTileColor(const AValue: TColor32);

    function GetShowTNE: Boolean;
    procedure SetShowTNE(const AValue: Boolean);

    function GetTNEColor: TColor32;
    procedure SetTNEColor(const AValue: TColor32);

    function GetSourceMap: IFillingMapMapsConfig;
    function GetStatic: IFillingMapLayerConfigStatic;

    function GetFillMode: TFillMode;
    procedure SetFillMode(const AValue: TFillMode);

    function GetFilterMode: Boolean;
    procedure SetFilterMode(const AValue: Boolean);

    function GetFillFirstDay: TDateTime;
    procedure SetFillFirstDay(const AValue: TDateTime);

    function GetFillLastDay: TDateTime;
    procedure SetFillLastDay(const AValue: TDateTime);

    function GetThreadConfig: IThreadConfig;
  public
    constructor Create(
      const AMainMap: IMapTypeChangeable;
      const AMapsSet: IMapTypeSet
    );
  end;

implementation

uses
  Classes,
  GR32,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_ConfigProviderHelpers,
  u_ThreadConfig,
  u_FillingMapMapsConfig,
  u_FillingMapLayerConfigStatic;

{ TFillingMapLayerConfig }

constructor TFillingMapLayerConfig.Create(
  const AMainMap: IMapTypeChangeable;
  const AMapsSet: IMapTypeSet
);
begin
  inherited Create;
  FVisible := False;
  FUseRelativeZoom := False;
  FZoom := 0;
  FShowTNE := True;
  FNoTileColor := SetAlpha(clBlack32, 110);
  FTNEColor := SetAlpha(clRed32, 110);
  FFillMode := fmUnexisting;
  FFilterMode := False;
  FFillFirstDay := EncodeDate(2000, 1, 1);
  FFillLastDay := DateOf(Now);
  FSourceMap :=
    TFillingMapMapsConfig.Create(
      AMainMap,
      AMapsSet
    );
  Add(FSourceMap, TConfigSaveLoadStrategyBasicUseProvider.Create);

  FThreadConfig := TThreadConfig.Create(tpLowest);
  Add(FThreadConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

function TFillingMapLayerConfig.CreateStatic: IInterface;
var
  VStatic: IFillingMapLayerConfigStatic;
begin
  VStatic :=
    TFillingMapLayerConfigStatic.Create(
      FVisible,
      FSourceMap.GetActiveMap.GetStatic,
      FSourceMap.GetActualMap,
      FUseRelativeZoom,
      FZoom,
      FNoTileColor,
      FShowTNE,
      FTNEColor,
      FFillMode,
      FFilterMode,
      FFillFirstDay,
      FFillLastDay
    );
  Result := VStatic;
end;

procedure TFillingMapLayerConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('Visible', FVisible);
    FUseRelativeZoom := AConfigData.ReadBool('RelativeZoom', FUseRelativeZoom);
    FZoom := AConfigData.ReadInteger('Zoom', FZoom);
    FShowTNE := AConfigData.ReadBool('ShowTNE', FShowTNE);
    FNoTileColor := ReadColor32(AConfigData, 'NoTileColor', FNoTileColor);
    FTNEColor := ReadColor32(AConfigData, 'TNEColor', FTNEColor);
    FFillMode := TFillMode(AConfigData.ReadInteger('FillingMapMode', Ord(FFillMode)));
    FFilterMode := AConfigData.ReadBool('DateFilter', FFilterMode);
    FFillFirstDay := AConfigData.ReadDate('FirstDay', FFillFirstDay);
    FFillLastDay := AConfigData.ReadDate('LastDay', FFillLastDay);

    SetChanged;
  end;
end;

procedure TFillingMapLayerConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('Visible', FVisible);
  AConfigData.WriteBool('RelativeZoom', FUseRelativeZoom);
  AConfigData.WriteInteger('Zoom', FZoom);
  AConfigData.WriteBool('ShowTNE', FShowTNE);
  WriteColor32(AConfigData, 'NoTileColor', FNoTileColor);
  WriteColor32(AConfigData, 'TNEColor', FTNEColor);
  AConfigData.WriteInteger('FillingMapMode', Ord(FFillMode));
  AConfigData.WriteBool('DateFilter', FFilterMode);
  AConfigData.WriteDate('FirstDay', FFillFirstDay);
  if abs(FFillLastDay - DateOf(Now)) > 0.001 then begin
    AConfigData.WriteDate('LastDay', FFillLastDay);
  end else begin
    AConfigData.DeleteValue('LastDay');
  end;
end;

function TFillingMapLayerConfig.GetNoTileColor: TColor32;
begin
  LockRead;
  try
    Result := FNoTileColor;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetShowTNE: Boolean;
begin
  LockRead;
  try
    Result := FShowTNE;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetSourceMap: IFillingMapMapsConfig;
begin
  Result := FSourceMap;
end;

function TFillingMapLayerConfig.GetZoom: Byte;
begin
  LockRead;
  try
    Result := FZoom;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetStatic: IFillingMapLayerConfigStatic;
begin
  Result := IFillingMapLayerConfigStatic(GetStaticInternal);
end;

function TFillingMapLayerConfig.GetThreadConfig: IThreadConfig;
begin
  Result := FThreadConfig;
end;

function TFillingMapLayerConfig.GetTNEColor: TColor32;
begin
  LockRead;
  try
    Result := FTNEColor;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetUseRelativeZoom: Boolean;
begin
  LockRead;
  try
    Result := FUseRelativeZoom;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetFillMode: TFillMode;
begin
  LockRead;
  try
    Result := FFillMode;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetFilterMode: Boolean;
begin
  LockRead;
  try
    Result := FFilterMode;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetFillFirstDay: TDateTime;
begin
  LockRead;
  try
    Result := FFillFirstDay;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetFillLastDay: TDateTime;
begin
  LockRead;
  try
    Result := FFillLastDay;
  finally
    UnlockRead;
  end;
end;

procedure TFillingMapLayerConfig.SetNoTileColor(const AValue: TColor32);
begin
  LockWrite;
  try
    if FNoTileColor <> AValue then begin
      FNoTileColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetShowTNE(const AValue: Boolean);
begin
  LockWrite;
  try
    if FShowTNE <> AValue then begin
      FShowTNE := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetZoom(const AValue: Byte);
begin
  LockWrite;
  try
    if FZoom <> AValue then begin
      FZoom := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetTNEColor(const AValue: TColor32);
begin
  LockWrite;
  try
    if FTNEColor <> AValue then begin
      FTNEColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetUseRelativeZoom(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUseRelativeZoom <> AValue then begin
      FUseRelativeZoom := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetVisible(const AValue: Boolean);
begin
  LockWrite;
  try
    if FVisible <> AValue then begin
      FVisible := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetFillMode(const AValue: TFillMode);
begin
  LockWrite;
  try
    if FFillMode <> AValue then begin
      FFillMode := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetFilterMode(const AValue: Boolean);
begin
  LockWrite;
  try
    if FFilterMode <> AValue then begin
      FFilterMode := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetFillFirstDay(const AValue: TDateTime);
begin
  LockWrite;
  try
    if FFillFirstDay <> AValue then begin
      FFillFirstDay := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetFillLastDay(const AValue: TDateTime);
begin
  LockWrite;
  try
    if FFillLastDay <> AValue then begin
      FFillLastDay := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
