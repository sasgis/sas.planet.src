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

unit u_PointCaptionsLayerConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_PointCaptionsLayerConfig,
  u_ConfigDataElementBase,
  u_BaseInterfacedObject;

type
  TPointCaptionsLayerConfigStatic = class(TBaseInterfacedObject, IPointCaptionsLayerConfigStatic)
  private
    FVisible: Boolean;
    FShowAzimuth: Boolean;
    FShowLastPointOnly: Boolean;
    FFontSize: Integer;
    FLastPointFontSize: Integer;
    FTextColor: TColor32;
    FTextBGColor: TColor32;
  private
    function GetVisible: Boolean;
    function GetShowAzimuth: Boolean;
    function GetShowLastPointOnly: Boolean;
    function GetFontSize: Integer;
    function GetLastPointFontSize: Integer;
    function GetTextColor: TColor32;
    function GetTextBGColor: TColor32;
  public
    constructor Create(
      AVisible: Boolean;
      AShowAzimuth: Boolean;
      AShowLastPointOnly: Boolean;
      AFontSize: Integer;
      ALastPointFontSize: Integer;
      ATextColor: TColor32;
      ATextBGColor: TColor32
    );
  end;

  TPointCaptionsLayerConfig = class(TConfigDataElementWithStaticBase, IPointCaptionsLayerConfig)
  private
    FVisible: Boolean;
    FShowAzimuth: Boolean;
    FShowLastPointOnly: Boolean;
    FFontSize: Integer;
    FLastPointFontSize: Integer;
    FTextColor: TColor32;
    FTextBGColor: TColor32;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    function GetShowAzimuth: Boolean;
    procedure SetShowAzimuth(AValue: Boolean);

    function GetShowLastPointOnly: Boolean;
    procedure SetShowLastPointOnly(const AValue: Boolean);

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);

    function GetLastPointFontSize: Integer;
    procedure SetLastPointFontSize(AValue: Integer);

    function GetTextColor: TColor32;
    procedure SetTextColor(const AValue: TColor32);

    function GetTextBGColor: TColor32;
    procedure SetTextBGColor(const AValue: TColor32);

    function GetStatic: IPointCaptionsLayerConfigStatic;
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigProviderHelpers;

{ TPointCaptionsLayerConfigStatic }

constructor TPointCaptionsLayerConfigStatic.Create(AVisible, AShowAzimuth,
  AShowLastPointOnly: Boolean; AFontSize, ALastPointFontSize: Integer;
  ATextColor, ATextBGColor: TColor32);
begin
  inherited Create;
  FVisible := AVisible;
  FShowAzimuth := AShowAzimuth;
  FShowLastPointOnly := AShowLastPointOnly;
  FFontSize := AFontSize;
  FLastPointFontSize := ALastPointFontSize;
  FTextColor := ATextColor;
  FTextBGColor := ATextBGColor;
end;

function TPointCaptionsLayerConfigStatic.GetFontSize: Integer;
begin
  Result := FFontSize;
end;

function TPointCaptionsLayerConfigStatic.GetLastPointFontSize: Integer;
begin
  Result := FLastPointFontSize;
end;

function TPointCaptionsLayerConfigStatic.GetShowLastPointOnly: Boolean;
begin
  Result := FShowLastPointOnly;
end;

function TPointCaptionsLayerConfigStatic.GetTextBGColor: TColor32;
begin
  Result := FTextBGColor;
end;

function TPointCaptionsLayerConfigStatic.GetTextColor: TColor32;
begin
  Result := FTextColor;
end;

function TPointCaptionsLayerConfigStatic.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TPointCaptionsLayerConfigStatic.GetShowAzimuth: Boolean;
begin
  Result := FShowAzimuth;
end;

{ TPointCaptionsLayerConfig }

constructor TPointCaptionsLayerConfig.Create;
begin
  inherited Create;
  FVisible := True;
  FShowAzimuth := True;
  FShowLastPointOnly := False;
  FFontSize := 7;
  FLastPointFontSize := 9;
  FTextColor := clBlack32;
  FTextBGColor := SetAlpha(ClWhite32, 110);
end;

function TPointCaptionsLayerConfig.CreateStatic: IInterface;
var
  VStatic: IPointCaptionsLayerConfigStatic;
begin
  VStatic :=
    TPointCaptionsLayerConfigStatic.Create(
      FVisible,
      FShowAzimuth,
      FShowLastPointOnly,
      FFontSize,
      FLastPointFontSize,
      FTextColor,
      FTextBGColor
    );
  Result := VStatic;
end;

procedure TPointCaptionsLayerConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('VisibleCaptions', FVisible);
    FShowAzimuth := AConfigData.ReadBool('ShowAzimuth', FShowAzimuth);
    FShowLastPointOnly := AConfigData.ReadBool('ShowLastPointCaptionOnly', FShowLastPointOnly);
    FFontSize := AConfigData.ReadInteger('FontSize', FFontSize);
    FLastPointFontSize := AConfigData.ReadInteger('LastPointFontSize', FLastPointFontSize);
    FTextColor := ReadColor32(AConfigData, 'TextColor', FTextColor);
    FTextBGColor := ReadColor32(AConfigData, 'TextBGColor', FTextBGColor);

    SetChanged;
  end;
end;

procedure TPointCaptionsLayerConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('VisibleCaptions', FVisible);
  AConfigData.WriteBool('ShowAzimuth', FShowAzimuth);
  AConfigData.WriteBool('ShowLastPointCaptionOnly', FShowLastPointOnly);
  AConfigData.WriteInteger('FontSize', FFontSize);
  AConfigData.WriteInteger('LastPointFontSize', FLastPointFontSize);

  WriteColor32(AConfigData, 'TextColor', FTextColor);
  WriteColor32(AConfigData, 'TextBGColor', FTextBGColor);
end;

function TPointCaptionsLayerConfig.GetFontSize: Integer;
begin
  LockRead;
  try
    Result := FFontSize;
  finally
    UnlockRead;
  end;
end;

function TPointCaptionsLayerConfig.GetLastPointFontSize: Integer;
begin
  LockRead;
  try
    Result := FLastPointFontSize;
  finally
    UnlockRead;
  end;
end;

function TPointCaptionsLayerConfig.GetShowLastPointOnly: Boolean;
begin
  LockRead;
  try
    Result := FShowLastPointOnly;
  finally
    UnlockRead;
  end;
end;

function TPointCaptionsLayerConfig.GetStatic: IPointCaptionsLayerConfigStatic;
begin
  Result := IPointCaptionsLayerConfigStatic(GetStaticInternal);
end;

function TPointCaptionsLayerConfig.GetTextBGColor: TColor32;
begin
  LockRead;
  try
    Result := FTextBGColor;
  finally
    UnlockRead;
  end;
end;

function TPointCaptionsLayerConfig.GetTextColor: TColor32;
begin
  LockRead;
  try
    Result := FTextColor;
  finally
    UnlockRead;
  end;
end;

function TPointCaptionsLayerConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

function TPointCaptionsLayerConfig.GetShowAzimuth: Boolean;
begin
  LockRead;
  try
    Result := FShowAzimuth;
  finally
    UnlockRead;
  end;
end;

procedure TPointCaptionsLayerConfig.SetFontSize(AValue: Integer);
begin
  LockWrite;
  try
    if FFontSize <> AValue then begin
      FFontSize := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TPointCaptionsLayerConfig.SetLastPointFontSize(AValue: Integer);
begin
  LockWrite;
  try
    if FLastPointFontSize <> AValue then begin
      FLastPointFontSize := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TPointCaptionsLayerConfig.SetShowLastPointOnly(
  const AValue: Boolean);
begin
  LockWrite;
  try
    if FShowLastPointOnly <> AValue then begin
      FShowLastPointOnly := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TPointCaptionsLayerConfig.SetTextBGColor(const AValue: TColor32);
begin
  LockWrite;
  try
    if FTextBGColor <> AValue then begin
      FTextBGColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TPointCaptionsLayerConfig.SetTextColor(const AValue: TColor32);
begin
  LockWrite;
  try
    if FTextColor <> AValue then begin
      FTextColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TPointCaptionsLayerConfig.SetVisible(AValue: Boolean);
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

procedure TPointCaptionsLayerConfig.SetShowAzimuth(AValue: Boolean);
begin
  LockWrite;
  try
    if FShowAzimuth <> AValue then begin
      FShowAzimuth := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
