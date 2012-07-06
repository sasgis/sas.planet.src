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

unit u_SelectionRectLayerConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_SelectionRectLayerConfig,
  u_ConfigDataElementBase;

type
  TSelectionRectLayerConfig = class(TConfigDataElementBase, ISelectionRectLayerConfig)
  private
    FFillColor: TColor32;
    FBorderColor: TColor32;
    FFontSize: Integer;
    FZoomDeltaColors: TArrayOfColor32;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetFillColor: TColor32;
    procedure SetFillColor(AValue: TColor32);

    function GetBorderColor: TColor32;
    procedure SetBorderColor(AValue: TColor32);

    function GetZoomDeltaCount: Integer;
    procedure SetZoomDeltaCount(AValue: Integer);

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);

    function GetZoomDeltaColor(AIndex: Integer): TColor32;
    procedure SetZoomDeltaColor(
      AIndex: Integer;
      AValue: TColor32
    );

    function GetZoomDeltaColors: TArrayOfColor32;
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigProviderHelpers;

{ TSelectionRectLayerConfig }

constructor TSelectionRectLayerConfig.Create;
var
  i: Integer;
  VColorComponent: Byte;
begin
  inherited Create;
  FFillColor := SetAlpha(clWhite32, 20);
  FBorderColor := SetAlpha(clBlue32, 150);
  FFontSize := 11;
  SetLength(FZoomDeltaColors, 3);
  for i := 0 to Length(FZoomDeltaColors) - 1 do begin
    VColorComponent := 256 shr i;
    FZoomDeltaColors[i] := Color32(VColorComponent - 1, VColorComponent - 1, VColorComponent - 1, 255);
  end;
end;

procedure TSelectionRectLayerConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    FFillColor := ReadColor32(AConfigData, 'FillColor', FFillColor);
    FBorderColor := ReadColor32(AConfigData, 'BorderColor', FBorderColor);
    FFontSize := AConfigData.ReadInteger('FontSize', FFontSize);

    SetChanged;
  end;
end;

procedure TSelectionRectLayerConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  WriteColor32(AConfigData, 'FillColor', FFillColor);
  WriteColor32(AConfigData, 'BorderColor', FBorderColor);
  AConfigData.WriteInteger('FontSize', FFontSize);
end;

function TSelectionRectLayerConfig.GetBorderColor: TColor32;
begin
  LockRead;
  try
    Result := FBorderColor;
  finally
    UnlockRead;
  end;
end;

function TSelectionRectLayerConfig.GetFillColor: TColor32;
begin
  LockRead;
  try
    Result := FFillColor;
  finally
    UnlockRead;
  end;
end;

function TSelectionRectLayerConfig.GetFontSize: Integer;
begin
  LockRead;
  try
    Result := FFontSize;
  finally
    UnlockRead;
  end;
end;

function TSelectionRectLayerConfig.GetZoomDeltaColor(AIndex: Integer): TColor32;
begin
  LockRead;
  try
    Result := FZoomDeltaColors[AIndex];
  finally
    UnlockRead;
  end;
end;

function TSelectionRectLayerConfig.GetZoomDeltaColors: TArrayOfColor32;
begin
  LockRead;
  try
    Result := FZoomDeltaColors;
  finally
    UnlockRead;
  end;
end;

function TSelectionRectLayerConfig.GetZoomDeltaCount: Integer;
begin
  LockRead;
  try
    Result := Length(FZoomDeltaColors);
  finally
    UnlockRead;
  end;
end;

procedure TSelectionRectLayerConfig.SetBorderColor(AValue: TColor32);
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

procedure TSelectionRectLayerConfig.SetFillColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FFillColor <> AValue then begin
      FFillColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSelectionRectLayerConfig.SetFontSize(AValue: Integer);
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

procedure TSelectionRectLayerConfig.SetZoomDeltaColor(
  AIndex: Integer;
  AValue: TColor32
);
var
  VColors: TArrayOfColor32;
begin
  LockWrite;
  try
    if FZoomDeltaColors[AIndex] <> AValue then begin
      VColors := copy(FZoomDeltaColors);
      VColors[AIndex] := AValue;
      FZoomDeltaColors := VColors;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSelectionRectLayerConfig.SetZoomDeltaCount(AValue: Integer);
begin
  LockWrite;
  try
    if Length(FZoomDeltaColors) <> AValue then begin
      SetLength(FZoomDeltaColors, AValue);
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
