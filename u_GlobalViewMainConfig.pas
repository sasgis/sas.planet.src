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
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetBackGroundColor: TColor;
    procedure SetBackGroundColor(const AValue: TColor);
  public
    constructor Create;
  end;

implementation

{ TGlobalViewMainConfig }

constructor TGlobalViewMainConfig.Create;
begin
  inherited Create;
  FBackGroundColor := clSilver;
end;

procedure TGlobalViewMainConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FBackGroundColor := TColor(AConfigData.ReadInteger('BackgroundColor', FBackGroundColor));
    SetChanged;
  end;
end;

procedure TGlobalViewMainConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
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

end.
