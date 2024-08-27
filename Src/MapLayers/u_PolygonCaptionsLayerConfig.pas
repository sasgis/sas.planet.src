{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_PolygonCaptionsLayerConfig;

interface

uses
  t_Bitmap32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_PolygonCaptionsLayerConfig,
  u_ConfigDataElementBase,
  u_BaseInterfacedObject;

type
  TPolygonCaptionsLayerConfigStatic = class(TBaseInterfacedObject, IPolygonCaptionsLayerConfigStatic)
  private
    FVisible: Boolean;
    FShowPerimeter: Boolean;
    FShowArea: Boolean;
    FFontSize: Integer;
    FFontName: string;
    FTextColor: TColor32;
    FTextBGColor: TColor32;
  private
    { IPolygonCaptionsLayerConfigStatic }
    function GetVisible: Boolean;
    function GetShowPerimeter: Boolean;
    function GetShowArea: Boolean;
    function GetFontSize: Integer;
    function GetFontName: string;
    function GetTextColor: TColor32;
    function GetTextBGColor: TColor32;
  public
    constructor Create(
      const AVisible: Boolean;
      const AShowPerimeter: Boolean;
      const AShowArea: Boolean;
      const AFontSize: Integer;
      const AFontName: string;
      const ATextColor: TColor32;
      const ATextBGColor: TColor32
    );
  end;

  TPolygonCaptionsLayerConfig = class(TConfigDataElementWithStaticBase, IPolygonCaptionsLayerConfig)
  private
    FVisible: Boolean;
    FShowPerimeter: Boolean;
    FShowArea: Boolean;
    FFontSize: Integer;
    FFontName: string;
    FTextColor: TColor32;
    FTextBGColor: TColor32;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { IPolygonCaptionsLayerConfig }
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    function GetShowPerimeter: Boolean;
    procedure SetShowPerimeter(const AValue: Boolean);

    function GetShowArea: Boolean;
    procedure SetShowArea(const AValue: Boolean);

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);

    function GetFontName: string;
    procedure SetFontName(const AValue: string);

    function GetTextColor: TColor32;
    procedure SetTextColor(const AValue: TColor32);

    function GetTextBGColor: TColor32;
    procedure SetTextBGColor(const AValue: TColor32);

    function GetStatic: IPolygonCaptionsLayerConfigStatic;
  public
    constructor Create;
  end;

implementation

uses
  GR32,
  u_ConfigProviderHelpers;

{ TPointCaptionsLayerConfigStatic }

constructor TPolygonCaptionsLayerConfigStatic.Create(
  const AVisible: Boolean;
  const AShowPerimeter: Boolean;
  const AShowArea: Boolean;
  const AFontSize: Integer;
  const AFontName: string;
  const ATextColor: TColor32;
  const ATextBGColor: TColor32
);
begin
  inherited Create;
  FVisible := AVisible;
  FShowPerimeter := AShowPerimeter;
  FShowArea := AShowArea;
  FFontSize := AFontSize;
  FFontName := AFontName;
  FTextColor := ATextColor;
  FTextBGColor := ATextBGColor;
end;

function TPolygonCaptionsLayerConfigStatic.GetFontName: string;
begin
  Result := FFontName;
end;

function TPolygonCaptionsLayerConfigStatic.GetFontSize: Integer;
begin
  Result := FFontSize;
end;

function TPolygonCaptionsLayerConfigStatic.GetShowArea: Boolean;
begin
  Result := FShowArea;
end;

function TPolygonCaptionsLayerConfigStatic.GetTextBGColor: TColor32;
begin
  Result := FTextBGColor;
end;

function TPolygonCaptionsLayerConfigStatic.GetTextColor: TColor32;
begin
  Result := FTextColor;
end;

function TPolygonCaptionsLayerConfigStatic.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TPolygonCaptionsLayerConfigStatic.GetShowPerimeter: Boolean;
begin
  Result := FShowPerimeter;
end;

{ TPointCaptionsLayerConfig }

constructor TPolygonCaptionsLayerConfig.Create;
begin
  inherited Create;
  FVisible := False;
  FShowPerimeter := False;
  FShowArea := True;
  FFontSize := 9;
  FFontName := 'Arial';
  FTextColor := clWhite32;
  FTextBGColor := SetAlpha(clBlack32, $70);
end;

function TPolygonCaptionsLayerConfig.CreateStatic: IInterface;
var
  VStatic: IPolygonCaptionsLayerConfigStatic;
begin
  VStatic :=
    TPolygonCaptionsLayerConfigStatic.Create(
      FVisible,
      FShowPerimeter,
      FShowArea,
      FFontSize,
      FFontName,
      FTextColor,
      FTextBGColor
    );
  Result := VStatic;
end;

procedure TPolygonCaptionsLayerConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('VisibleCaptions', FVisible);
    FShowPerimeter := AConfigData.ReadBool('ShowPerimeter', FShowPerimeter);
    FShowArea := AConfigData.ReadBool('ShowArea', FShowArea);
    FFontSize := AConfigData.ReadInteger('FontSize', FFontSize);
    FFontName := AConfigData.ReadString('FontName', FFontName);
    FTextColor := ReadColor32(AConfigData, 'TextColor', FTextColor);
    FTextBGColor := ReadColor32(AConfigData, 'TextBGColor', FTextBGColor);

    SetChanged;
  end;
end;

procedure TPolygonCaptionsLayerConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('VisibleCaptions', FVisible);
  AConfigData.WriteBool('ShowPerimeter', FShowPerimeter);
  AConfigData.WriteBool('ShowArea', FShowArea);
  AConfigData.WriteInteger('FontSize', FFontSize);
  AConfigData.WriteString('FontName', FFontName);

  WriteColor32(AConfigData, 'TextColor', FTextColor);
  WriteColor32(AConfigData, 'TextBGColor', FTextBGColor);
end;

function TPolygonCaptionsLayerConfig.GetFontName: string;
begin
  LockRead;
  try
    Result := FFontName;
  finally
    UnlockRead;
  end;
end;

function TPolygonCaptionsLayerConfig.GetFontSize: Integer;
begin
  LockRead;
  try
    Result := FFontSize;
  finally
    UnlockRead;
  end;
end;

function TPolygonCaptionsLayerConfig.GetShowArea: Boolean;
begin
  LockRead;
  try
    Result := FShowArea;
  finally
    UnlockRead;
  end;
end;

function TPolygonCaptionsLayerConfig.GetStatic: IPolygonCaptionsLayerConfigStatic;
begin
  Result := IPolygonCaptionsLayerConfigStatic(GetStaticInternal);
end;

function TPolygonCaptionsLayerConfig.GetTextBGColor: TColor32;
begin
  LockRead;
  try
    Result := FTextBGColor;
  finally
    UnlockRead;
  end;
end;

function TPolygonCaptionsLayerConfig.GetTextColor: TColor32;
begin
  LockRead;
  try
    Result := FTextColor;
  finally
    UnlockRead;
  end;
end;

function TPolygonCaptionsLayerConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

function TPolygonCaptionsLayerConfig.GetShowPerimeter: Boolean;
begin
  LockRead;
  try
    Result := FShowPerimeter;
  finally
    UnlockRead;
  end;
end;

procedure TPolygonCaptionsLayerConfig.SetFontName(const AValue: string);
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

procedure TPolygonCaptionsLayerConfig.SetFontSize(AValue: Integer);
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

procedure TPolygonCaptionsLayerConfig.SetShowArea(const AValue: Boolean);
begin
  LockWrite;
  try
    if FShowArea <> AValue then begin
      FShowArea := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TPolygonCaptionsLayerConfig.SetTextBGColor(const AValue: TColor32);
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

procedure TPolygonCaptionsLayerConfig.SetTextColor(const AValue: TColor32);
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

procedure TPolygonCaptionsLayerConfig.SetVisible(AValue: Boolean);
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

procedure TPolygonCaptionsLayerConfig.SetShowPerimeter(const AValue: Boolean);
begin
  LockWrite;
  try
    if FShowPerimeter <> AValue then begin
      FShowPerimeter := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
