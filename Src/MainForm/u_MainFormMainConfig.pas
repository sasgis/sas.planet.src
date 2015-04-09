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

unit u_MainFormMainConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MainFormConfig,
  u_ConfigDataElementBase;

type
  TMainFormMainConfig = class(TConfigDataElementBase, IMainFormMainConfig)
  private
    FShowMapName: Boolean;
    FDisableZoomingByMouseScroll: Boolean;
    FMouseScrollInvert: Boolean;
    FShowHintOnMarks: Boolean;
    FShowHintOnlyInMapMoveMode: Boolean;
    FMagnetDraw: Boolean;
    FMagnetDrawSize: Integer;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetShowMapName: Boolean;
    procedure SetShowMapName(AValue: Boolean);

    function GetDisableZoomingByMouseScroll: Boolean;
    procedure SetDisableZoomingByMouseScroll(AValue: Boolean);

    function GetMouseScrollInvert: Boolean;
    procedure SetMouseScrollInvert(AValue: Boolean);

    function GetShowHintOnMarks: Boolean;
    procedure SetShowHintOnMarks(AValue: Boolean);

    function GetShowHintOnlyInMapMoveMode: Boolean;
    procedure SetShowHintOnlyInMapMoveMode(AValue: Boolean);

    function GetMagnetDraw: Boolean;
    procedure SetMagnetDraw(AValue: Boolean);

    function GetMagnetDrawSize: Integer;
    procedure SetMagnetDrawSize(AValue: Integer);
  public
    constructor Create;
  end;

implementation

{ TMainFormMainConfig }

constructor TMainFormMainConfig.Create;
begin
  inherited Create;
  FMagnetDraw := True;
  FMagnetDrawSize := 10;
  FShowMapName := True;
  FMouseScrollInvert := False;
  FDisableZoomingByMouseScroll := False;
  FShowHintOnMarks := True;
  FShowHintOnlyInMapMoveMode := False;
end;

procedure TMainFormMainConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FShowMapName := AConfigData.ReadBool('ShowMapNameOnPanel', FShowMapName);
    FDisableZoomingByMouseScroll := AConfigData.ReadBool('DisableZoomingByMouseScroll', FDisableZoomingByMouseScroll);
    FMouseScrollInvert := AConfigData.ReadBool('MouseScrollInvert', FMouseScrollInvert);
    FShowHintOnMarks := AConfigData.ReadBool('ShowHintOnMarks', FShowHintOnMarks);
    FShowHintOnlyInMapMoveMode := AConfigData.ReadBool('ShowHintOnlyInMapMoveMode', FShowHintOnlyInMapMoveMode);
    FMagnetDraw := AConfigData.ReadBool('MagnetDraw', FMagnetDraw);
    FMagnetDrawSize := AConfigData.ReadInteger('MagnetDrawSize', FMagnetDrawSize);

    SetChanged;
  end;
end;

procedure TMainFormMainConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('ShowMapNameOnPanel', FShowMapName);
  AConfigData.WriteBool('DisableZoomingByMouseScroll', FDisableZoomingByMouseScroll);
  AConfigData.WriteBool('MouseScrollInvert', FMouseScrollInvert);
  AConfigData.WriteBool('ShowHintOnMarks', FShowHintOnMarks);
  AConfigData.WriteBool('ShowHintOnlyInMapMoveMode', FShowHintOnlyInMapMoveMode);
  AConfigData.WriteBool('MagnetDraw', FMagnetDraw);
  AConfigData.WriteInteger('MagnetDrawSize', FMagnetDrawSize);
end;

function TMainFormMainConfig.GetDisableZoomingByMouseScroll: Boolean;
begin
  LockRead;
  try
    Result := FDisableZoomingByMouseScroll;
  finally
    UnlockRead;
  end;
end;

function TMainFormMainConfig.GetMagnetDraw: Boolean;
begin
  LockRead;
  try
    Result := FMagnetDraw;
  finally
    UnlockRead;
  end;
end;

function TMainFormMainConfig.GetMagnetDrawSize: Integer;
begin
  LockRead;
  try
    Result := FMagnetDrawSize;
  finally
    UnlockRead;
  end;
end;

function TMainFormMainConfig.GetMouseScrollInvert: Boolean;
begin
  LockRead;
  try
    Result := FMouseScrollInvert;
  finally
    UnlockRead;
  end;
end;

function TMainFormMainConfig.GetShowHintOnlyInMapMoveMode: Boolean;
begin
  LockRead;
  try
    Result := FShowHintOnlyInMapMoveMode;
  finally
    UnlockRead;
  end;
end;

function TMainFormMainConfig.GetShowHintOnMarks: Boolean;
begin
  LockRead;
  try
    Result := FShowHintOnMarks;
  finally
    UnlockRead;
  end;
end;

function TMainFormMainConfig.GetShowMapName: Boolean;
begin
  LockRead;
  try
    Result := FShowMapName;
  finally
    UnlockRead;
  end;
end;

procedure TMainFormMainConfig.SetDisableZoomingByMouseScroll(AValue: Boolean);
begin
  LockWrite;
  try
    if FDisableZoomingByMouseScroll <> AValue then begin
      FDisableZoomingByMouseScroll := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainFormMainConfig.SetMagnetDraw(AValue: Boolean);
begin
  LockWrite;
  try
    if FMagnetDraw <> AValue then begin
      FMagnetDraw := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainFormMainConfig.SetMagnetDrawSize(AValue: Integer);
begin
  LockWrite;
  try
    if FMagnetDrawSize <> AValue then begin
      FMagnetDrawSize := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainFormMainConfig.SetMouseScrollInvert(AValue: Boolean);
begin
  LockWrite;
  try
    if FMouseScrollInvert <> AValue then begin
      FMouseScrollInvert := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainFormMainConfig.SetShowHintOnlyInMapMoveMode(AValue: Boolean);
begin
  LockWrite;
  try
    if FShowHintOnlyInMapMoveMode <> AValue then begin
      FShowHintOnlyInMapMoveMode := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainFormMainConfig.SetShowHintOnMarks(AValue: Boolean);
begin
  LockWrite;
  try
    if FShowHintOnMarks <> AValue then begin
      FShowHintOnMarks := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainFormMainConfig.SetShowMapName(AValue: Boolean);
begin
  LockWrite;
  try
    if FShowMapName <> AValue then begin
      FShowMapName := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
