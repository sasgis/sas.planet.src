{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_GlobalViewMainConfig;

interface

uses
  Graphics,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GlobalViewMainConfig,
  u_ConfigDataElementBase;

type
  TGlobalViewMainConfig = class(TConfigDataElementBase, IGlobalViewMainConfig)
  private
    FBackGroundColor: TColor;
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetBackGroundColor: TColor;
    procedure SetBackGroundColor(const AValue: TColor);

    function GetUsePrevZoomAtMap: Boolean;
    procedure SetUsePrevZoomAtMap(const AValue: Boolean);

    function GetUsePrevZoomAtLayer: Boolean;
    procedure SetUsePrevZoomAtLayer(const AValue: Boolean);
  public
    constructor Create;
  end;

implementation

{ TGlobalViewMainConfig }

constructor TGlobalViewMainConfig.Create;
begin
  inherited;
  FBackGroundColor := clSilver;
  FUsePrevZoomAtMap := True;
  FUsePrevZoomAtLayer := True;
end;

procedure TGlobalViewMainConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FUsePrevZoomAtMap := AConfigData.ReadBool('UsePrevZoomAtMap', FUsePrevZoomAtMap);
    FUsePrevZoomAtLayer := AConfigData.ReadBool('UsePrevZoomAtLayer', FUsePrevZoomAtLayer);
    FBackGroundColor := TColor(AConfigData.ReadInteger('BackgroundColor', FBackGroundColor));
    SetChanged;
  end;
end;

procedure TGlobalViewMainConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('UsePrevZoomAtMap', FUsePrevZoomAtMap);
  AConfigData.WriteBool('UsePrevZoomAtLayer', FUsePrevZoomAtLayer);
  AConfigData.WriteInteger('BackgroundColor', Integer(FBackGroundColor));
end;

function TGlobalViewMainConfig.GetBackGroundColor: TColor;
begin
  LockRead;
  try
    Result := FBackGroundColor;
  finally
    UnlockRead;
  end;
end;

function TGlobalViewMainConfig.GetUsePrevZoomAtLayer: Boolean;
begin
  LockRead;
  try
    Result := FUsePrevZoomAtLayer;
  finally
    UnlockRead;
  end;
end;

function TGlobalViewMainConfig.GetUsePrevZoomAtMap: Boolean;
begin
  LockRead;
  try
    Result := FUsePrevZoomAtMap;
  finally
    UnlockRead;
  end;
end;

procedure TGlobalViewMainConfig.SetBackGroundColor(const AValue: TColor);
begin
  LockWrite;
  try
    if FBackGroundColor <> AValue then begin
      FBackGroundColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGlobalViewMainConfig.SetUsePrevZoomAtLayer(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUsePrevZoomAtLayer <> AValue then begin
      FUsePrevZoomAtLayer := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGlobalViewMainConfig.SetUsePrevZoomAtMap(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUsePrevZoomAtMap <> AValue then begin
      FUsePrevZoomAtMap := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
