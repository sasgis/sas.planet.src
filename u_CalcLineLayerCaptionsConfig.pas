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

unit u_CalcLineLayerCaptionsConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_CalcLineLayerConfig,
  u_ConfigDataElementBase;

type
  TCalcLineLayerCaptionsConfig = class(TConfigDataElementBase, ICalcLineLayerCaptionsConfig)
  private
    FLenShow: Boolean;
    FTextColor: TColor32;
    FTextBGColor: TColor32;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetLenShow: Boolean;
    procedure SetLenShow(const AValue: Boolean);

    function GetTextColor: TColor32;
    procedure SetTextColor(const AValue: TColor32);

    function GetTextBGColor: TColor32;
    procedure SetTextBGColor(const AValue: TColor32);
  public
    constructor Create;
  end;


implementation

uses
  u_ConfigProviderHelpers;

{ TCalcLineLayerCaptionsConfig }

constructor TCalcLineLayerCaptionsConfig.Create;
begin
  inherited;
  LockWrite;
  try
    SetLenShow(True);

    SetTextColor(clBlack32);
    SetTextBGColor(SetAlpha(ClWhite32, 110));
  finally
    UnlockWrite;
  end;
end;

procedure TCalcLineLayerCaptionsConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FLenShow := AConfigData.ReadBool('LenShow', FLenShow);

    FTextColor := ReadColor32(AConfigData, 'TextColor', FTextColor);
    FTextBGColor := ReadColor32(AConfigData, 'TextBGColor', FTextBGColor);

    SetChanged;
  end;
end;

procedure TCalcLineLayerCaptionsConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('LenShow', FLenShow);

  WriteColor32(AConfigData, 'TextColor', FTextColor);
  WriteColor32(AConfigData, 'TextBGColor', FTextBGColor);
end;

function TCalcLineLayerCaptionsConfig.GetLenShow: Boolean;
begin
  LockRead;
  try
    Result := FLenShow;
  finally
    UnlockRead;
  end;
end;

function TCalcLineLayerCaptionsConfig.GetTextBGColor: TColor32;
begin
  LockRead;
  try
    Result := FTextBGColor;
  finally
    UnlockRead;
  end;
end;

function TCalcLineLayerCaptionsConfig.GetTextColor: TColor32;
begin
  LockRead;
  try
    Result := FTextColor;
  finally
    UnlockRead;
  end;
end;

procedure TCalcLineLayerCaptionsConfig.SetLenShow(const AValue: Boolean);
begin
  LockWrite;
  try
    if FLenShow <> AValue then begin
      FLenShow := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TCalcLineLayerCaptionsConfig.SetTextBGColor(const AValue: TColor32);
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

procedure TCalcLineLayerCaptionsConfig.SetTextColor(const AValue: TColor32);
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

end.
