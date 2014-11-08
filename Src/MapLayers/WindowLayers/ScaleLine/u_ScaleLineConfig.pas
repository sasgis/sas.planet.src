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

unit u_ScaleLineConfig;

interface

uses
  t_Bitmap32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ScaleLineConfig,
  u_ConfigDataElementBase;

type
  TScaleLineConfig = class(TConfigDataElementBase, IScaleLineConfig)
  private
    FVisible: Boolean;
    FExtended: Boolean;
    FWidth: Integer;
    FColor: TColor32;
    FOutLineColor: TColor32;
    FFontName: string;
    FFontSize: Integer;
    FNumbersFormat: TScaleLegendNumbersFormat;
    FBottomMargin: Integer;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    function GetExtended: Boolean;
    procedure SetExtended(AValue: Boolean);

    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);

    function GetColor: TColor32;
    procedure SetColor(AValue: TColor32);

    function GetOutLineColor: TColor32;
    procedure SetOutLineColor(AValue: TColor32);

    function GetFontName: string;
    procedure SetFontName(const AValue: string);

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);

    function GetNumbersFormat: TScaleLegendNumbersFormat;
    procedure SetNumbersFormat(AValue: TScaleLegendNumbersFormat);

    function GetBottomMargin: Integer;
    procedure SetBottomMargin(AValue: Integer);
  public
    constructor Create;
  end;

implementation

uses
  GR32,
  u_ConfigProviderHelpers;

const
  CDefaultNumbersFormat: TScaleLegendNumbersFormat = slnfNice;

{ TScaleLineConfig }

constructor TScaleLineConfig.Create;
begin
  inherited Create;
  FVisible := True;
  FExtended := False;
  FWidth := 256;
  FColor := SetAlpha(clWhite32, 255);
  FOutLineColor := SetAlpha(clBlack32, 170);
  FFontName := 'Arial';
  FFontSize := 8;
  FNumbersFormat := CDefaultNumbersFormat;
  FBottomMargin := 0;
end;

procedure TScaleLineConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('Visible', FVisible);
    FExtended := AConfigData.ReadBool('Extended', FExtended);
    FWidth := AConfigData.ReadInteger('Width', FWidth);
    FColor := ReadColor32(AConfigData, 'Color', FColor);
    FOutLineColor := ReadColor32(AConfigData, 'OutLineColor', FOutLineColor);
    FFontName := AConfigData.ReadString('FontName', FFontName);
    FFontSize := AConfigData.ReadInteger('FontSize', FFontSize);
    case AConfigData.ReadInteger('NumbersFormat', Integer(FNumbersFormat)) of
      Integer(slnfScienceRound): begin
        FNumbersFormat := slnfScienceRound;
      end;
      Integer(slnfScience): begin
        FNumbersFormat := slnfScience;
      end;
    else begin
      FNumbersFormat := slnfNice;
    end;
    end;
    SetChanged;
  end;
end;

procedure TScaleLineConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Visible', FVisible);
  AConfigData.WriteBool('Extended', FExtended);
  AConfigData.WriteInteger('Width', FWidth);
  WriteColor32(AConfigData, 'Color', FColor);
  WriteColor32(AConfigData, 'OutLineColor', FOutLineColor);
  AConfigData.WriteString('FontName', FFontName);
  AConfigData.WriteInteger('FontSize', FFontSize);
  AConfigData.WriteInteger('NumbersFormat', Integer(FNumbersFormat));
end;

{$REGION 'GET Property Value Methods'}

function TScaleLineConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

function TScaleLineConfig.GetExtended: Boolean;
begin
  LockRead;
  try
    Result := FExtended;
  finally
    UnlockRead;
  end;
end;

function TScaleLineConfig.GetWidth: Integer;
begin
  LockRead;
  try
    Result := FWidth;
  finally
    UnlockRead;
  end;
end;

function TScaleLineConfig.GetColor: TColor32;
begin
  LockRead;
  try
    Result := FColor;
  finally
    UnlockRead;
  end;
end;

function TScaleLineConfig.GetOutLineColor: TColor32;
begin
  LockRead;
  try
    Result := FOutLineColor;
  finally
    UnlockRead;
  end;
end;

function TScaleLineConfig.GetFontName: string;
begin
  LockRead;
  try
    Result := FFontName;
  finally
    UnlockRead;
  end;
end;

function TScaleLineConfig.GetFontSize: Integer;
begin
  LockRead;
  try
    Result := FFontSize;
  finally
    UnlockRead;
  end;
end;

function TScaleLineConfig.GetNumbersFormat: TScaleLegendNumbersFormat;
begin
  LockRead;
  try
    Result := FNumbersFormat;
  finally
    UnlockRead;
  end;
end;

function TScaleLineConfig.GetBottomMargin: Integer;
begin
  LockRead;
  try
    Result := FBottomMargin;
  finally
    UnlockRead;
  end;
end;

{$ENDREGION}//GET Property Value Methods

{$REGION 'SET Property Value Methods'}

procedure TScaleLineConfig.SetVisible(AValue: Boolean);
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

procedure TScaleLineConfig.SetExtended(AValue: Boolean);
begin
  LockWrite;
  try
    if FExtended <> AValue then begin
      FExtended := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TScaleLineConfig.SetWidth(AValue: Integer);
begin
  LockWrite;
  try
    if FWidth <> AValue then begin
      FWidth := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TScaleLineConfig.SetColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FColor <> AValue then begin
      FColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TScaleLineConfig.SetOutLineColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FOutLineColor <> AValue then begin
      FOutLineColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TScaleLineConfig.SetFontName(const AValue: string);
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

procedure TScaleLineConfig.SetFontSize(AValue: Integer);
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

procedure TScaleLineConfig.SetNumbersFormat(AValue: TScaleLegendNumbersFormat);
begin
  LockWrite;
  try
    if FNumbersFormat <> AValue then begin
      FNumbersFormat := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TScaleLineConfig.SetBottomMargin(AValue: Integer);
begin
  LockWrite;
  try
    if FBottomMargin <> AValue then begin
      FBottomMargin := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

{$ENDREGION}//SET Property Value Methods

end.
