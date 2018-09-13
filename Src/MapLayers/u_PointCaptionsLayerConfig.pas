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

unit u_PointCaptionsLayerConfig;

interface

uses
  t_Bitmap32,
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
    FShowIntermediateDist: Boolean;
    FShowDistIncrement: Boolean;
    FFontSize: Integer;
    FLastPointFontSize: Integer;
    FFontName: string;
    FTextColor: TColor32;
    FTextBGColor: TColor32;
  private
    function GetVisible: Boolean;
    function GetShowAzimuth: Boolean;
    function GetShowIntermediateDist: Boolean;
    function GetShowDistIncrement: Boolean;
    function GetFontSize: Integer;
    function GetLastPointFontSize: Integer;
    function GetFontName: string;
    function GetTextColor: TColor32;
    function GetTextBGColor: TColor32;
  public
    constructor Create(
      const AVisible: Boolean;
      const AShowAzimuth: Boolean;
      const AShowIntermediateDist: Boolean;
      const AShowDistIncrement: Boolean;
      const AFontSize: Integer;
      const ALastPointFontSize: Integer;
      const AFontName: string;
      const ATextColor: TColor32;
      const ATextBGColor: TColor32
    );
  end;

  TPointCaptionsLayerConfig = class(TConfigDataElementWithStaticBase, IPointCaptionsLayerConfig)
  private
    FVisible: Boolean;
    FShowAzimuth: Boolean;
    FShowIntermediateDist: Boolean;
    FShowDistIncrement: Boolean;
    FFontSize: Integer;
    FLastPointFontSize: Integer;
    FFontName: string;
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

    function GetShowIntermediateDist: Boolean;
    procedure SetShowIntermediateDist(const AValue: Boolean);

    function GetShowDistIncrement: Boolean;
    procedure SetShowDistIncrement(const AValue: Boolean);

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);

    function GetLastPointFontSize: Integer;
    procedure SetLastPointFontSize(AValue: Integer);

    function GetFontName: string;
    procedure SetFontName(const AValue: string);

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
  GR32,
  u_ConfigProviderHelpers;

{ TPointCaptionsLayerConfigStatic }

constructor TPointCaptionsLayerConfigStatic.Create(
  const AVisible: Boolean;
  const AShowAzimuth: Boolean;
  const AShowIntermediateDist: Boolean;
  const AShowDistIncrement: Boolean;
  const AFontSize: Integer;
  const ALastPointFontSize: Integer;
  const AFontName: string;
  const ATextColor: TColor32;
  const ATextBGColor: TColor32
);
begin
  inherited Create;
  FVisible := AVisible;
  FShowAzimuth := AShowAzimuth;
  FShowIntermediateDist := AShowIntermediateDist;
  FShowDistIncrement := AShowDistIncrement;
  FFontSize := AFontSize;
  FLastPointFontSize := ALastPointFontSize;
  FFontName := AFontName;
  FTextColor := ATextColor;
  FTextBGColor := ATextBGColor;
end;

function TPointCaptionsLayerConfigStatic.GetFontName: string;
begin
  Result := FFontName;
end;

function TPointCaptionsLayerConfigStatic.GetFontSize: Integer;
begin
  Result := FFontSize;
end;

function TPointCaptionsLayerConfigStatic.GetLastPointFontSize: Integer;
begin
  Result := FLastPointFontSize;
end;

function TPointCaptionsLayerConfigStatic.GetShowIntermediateDist: Boolean;
begin
  Result := FShowIntermediateDist;
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

function TPointCaptionsLayerConfigStatic.GetShowDistIncrement: Boolean;
begin
  Result := FShowDistIncrement;
end;

{ TPointCaptionsLayerConfig }

constructor TPointCaptionsLayerConfig.Create;
begin
  inherited Create;
  FVisible := True;
  FShowAzimuth := True;
  FShowIntermediateDist := True;
  FShowDistIncrement := True;
  FFontSize := 8;
  FLastPointFontSize := 9;
  FFontName := 'Arial';
  FTextColor := clWhite32;
  FTextBGColor := SetAlpha(clBlack32, $70);
end;

function TPointCaptionsLayerConfig.CreateStatic: IInterface;
var
  VStatic: IPointCaptionsLayerConfigStatic;
begin
  VStatic :=
    TPointCaptionsLayerConfigStatic.Create(
      FVisible,
      FShowAzimuth,
      FShowIntermediateDist,
      FShowDistIncrement,
      FFontSize,
      FLastPointFontSize,
      FFontName,
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
    FShowIntermediateDist := AConfigData.ReadBool('ShowIntermediateDist', FShowIntermediateDist);
    FShowDistIncrement := AConfigData.ReadBool('ShowDistIncrement', FShowDistIncrement);
    FFontSize := AConfigData.ReadInteger('FontSize', FFontSize);
    FLastPointFontSize := AConfigData.ReadInteger('LastPointFontSize', FLastPointFontSize);
    FFontName := AConfigData.ReadString('FontName', FFontName);
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
  AConfigData.WriteBool('ShowIntermediateDist', FShowIntermediateDist);
  AConfigData.WriteBool('ShowDistIncrement', FShowDistIncrement);
  AConfigData.WriteInteger('FontSize', FFontSize);
  AConfigData.WriteInteger('LastPointFontSize', FLastPointFontSize);
  AConfigData.WriteString('FontName', FFontName);

  WriteColor32(AConfigData, 'TextColor', FTextColor);
  WriteColor32(AConfigData, 'TextBGColor', FTextBGColor);
end;

function TPointCaptionsLayerConfig.GetFontName: string;
begin
  LockRead;
  try
    Result := FFontName;
  finally
    UnlockRead;
  end;
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

function TPointCaptionsLayerConfig.GetShowIntermediateDist: Boolean;
begin
  LockRead;
  try
    Result := FShowIntermediateDist;
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

function TPointCaptionsLayerConfig.GetShowDistIncrement: Boolean;
begin
  LockRead;
  try
    Result := FShowDistIncrement;
  finally
    UnlockRead;
  end;
end;

procedure TPointCaptionsLayerConfig.SetFontName(const AValue: string);
begin
  LockWrite;
  try
    if FFontName <> AValue then begin
      FFontName := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
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

procedure TPointCaptionsLayerConfig.SetShowIntermediateDist(const AValue: Boolean);
begin
  LockWrite;
  try
    if FShowIntermediateDist <> AValue then begin
      FShowIntermediateDist := AValue;
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

procedure TPointCaptionsLayerConfig.SetShowDistIncrement(const AValue: Boolean);
begin
  LockWrite;
  try
    if FShowDistIncrement <> AValue then begin
      FShowDistIncrement := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
