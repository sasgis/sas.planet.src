{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_MainWindowPositionConfig;

interface

uses
  Types,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase,
  i_MainWindowPosition;

type
  TMainWindowPositionConfig = class(TConfigDataElementBase, IMainWindowPosition)
  private
    FIsFullScreen: Boolean;
    FIsMaximized: Boolean;
    FIsMinimized: Boolean;
    FIsBordersVisible: Boolean;
    FBoundsRect: TRect;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetIsFullScreen: Boolean;
    function GetIsMaximized: Boolean;
    function GetBoundsRect: TRect;
    procedure ToggleFullScreen;
    procedure SetFullScreen;
    procedure SetNoFullScreen;
    procedure SetMaximized;
    procedure SetNormalWindow;
    procedure SetWindowPosition(const ARect: TRect);
    function GetIsMinimized: Boolean;
    procedure SetNotMinimized;
    procedure SetMinimized;
    function GetIsBordersVisible: Boolean;
    procedure ToggleBordersVisible;
  public
    constructor Create(const AStartRect: TRect);
  end;

implementation

{ TMainWindowPositionConfig }

constructor TMainWindowPositionConfig.Create(const AStartRect: TRect);
begin
  inherited Create;
  FBoundsRect := AStartRect;
  FIsFullScreen := False;
  FIsMaximized := False;
  FIsMinimized := False;
  FIsBordersVisible := True;
end;

procedure TMainWindowPositionConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FBoundsRect := Bounds(
      AConfigData.ReadInteger('Left', FBoundsRect.Left),
      AConfigData.ReadInteger('Top', FBoundsRect.Top),
      AConfigData.ReadInteger('Width', FBoundsRect.Right - FBoundsRect.Top),
      AConfigData.ReadInteger('Height', FBoundsRect.Bottom - FBoundsRect.Top)
    );
    FIsMaximized := AConfigData.ReadBool('Maximized', FIsMaximized);
    FIsFullScreen := AConfigData.ReadBool('FullScreen', FIsFullScreen);
    FIsBordersVisible := AConfigData.ReadBool('BordersVisible', FIsBordersVisible);
    SetChanged;
  end;
end;

procedure TMainWindowPositionConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('FullScreen', FIsFullScreen);
  AConfigData.WriteBool('Maximized', FIsMaximized);
  AConfigData.WriteBool('BordersVisible', FIsBordersVisible);
  AConfigData.WriteInteger('Left', FBoundsRect.Left);
  AConfigData.WriteInteger('Top', FBoundsRect.Top);
  AConfigData.WriteInteger('Width', FBoundsRect.Right - FBoundsRect.Left);
  AConfigData.WriteInteger('Height', FBoundsRect.Bottom - FBoundsRect.Top);
end;

function TMainWindowPositionConfig.GetBoundsRect: TRect;
begin
  LockRead;
  try
    Result := FBoundsRect;
  finally
    UnlockRead;
  end;
end;

function TMainWindowPositionConfig.GetIsFullScreen: Boolean;
begin
  LockRead;
  try
    Result := FIsFullScreen;
  finally
    UnlockRead;
  end;
end;

function TMainWindowPositionConfig.GetIsMaximized: Boolean;
begin
  LockRead;
  try
    Result := FIsMaximized;
  finally
    UnlockRead;
  end;
end;

function TMainWindowPositionConfig.GetIsMinimized: Boolean;
begin
  LockRead;
  try
    Result := FIsMinimized;
  finally
    UnlockRead;
  end;
end;

function TMainWindowPositionConfig.GetIsBordersVisible: Boolean;
begin
  LockRead;
  try
    Result := FIsBordersVisible;
  finally
    UnlockRead;
  end;
end;

procedure TMainWindowPositionConfig.SetFullScreen;
begin
  LockWrite;
  try
    if not FIsFullScreen then begin
      FIsFullScreen := True;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainWindowPositionConfig.SetMaximized;
begin
  LockWrite;
  try
    if FIsFullScreen or not FIsMaximized then begin
      FIsFullScreen := False;
      FIsMaximized := True;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainWindowPositionConfig.SetMinimized;
begin
  LockWrite;
  try
    if not FIsMinimized then begin
      FIsMinimized := True;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainWindowPositionConfig.SetNoFullScreen;
begin
  LockWrite;
  try
    if FIsFullScreen then begin
      FIsFullScreen := False;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainWindowPositionConfig.ToggleBordersVisible;
begin
  LockWrite;
  try
    FIsBordersVisible := not FIsBordersVisible;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TMainWindowPositionConfig.ToggleFullScreen;
begin
  LockWrite;
  try
    FIsFullScreen := not FIsFullScreen;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TMainWindowPositionConfig.SetNormalWindow;
begin
  LockWrite;
  try
    if FIsFullScreen or FIsMaximized then begin
      FIsFullScreen := False;
      FIsMaximized := False;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainWindowPositionConfig.SetNotMinimized;
begin
  LockWrite;
  try
    if FIsMinimized then begin
      FIsMinimized := False;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainWindowPositionConfig.SetWindowPosition(const ARect: TRect);
begin
  LockWrite;
  try
    if FIsFullScreen or FIsMaximized then begin
      FIsFullScreen := False;
      FIsMaximized := False;
      SetChanged;
    end;
    if not EqualRect(FBoundsRect, ARect) then begin
      FBoundsRect := ARect;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
