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

unit u_ViewProjectionConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ViewProjectionConfig,
  u_ConfigDataElementBase;

type
  TViewProjectionConfig = class(TConfigDataElementBase, IViewProjectionConfig)
  private
    FEPSG: Integer;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetEPSG: Integer;
    procedure SetEPSG(AValue: Integer);
  public
    constructor Create;
  end;

implementation

{ TViewProjectionConfig }

constructor TViewProjectionConfig.Create;
begin
  inherited Create;
  FEPSG := 0;
end;

procedure TViewProjectionConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FEPSG := AConfigData.ReadInteger('EPSG', FEPSG);
    SetChanged;
  end;
end;

procedure TViewProjectionConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteInteger('EPSG', FEPSG);
end;

function TViewProjectionConfig.GetEPSG: Integer;
begin
  LockRead;
  try
    Result := FEPSG;
  finally
    UnlockRead;
  end;
end;

procedure TViewProjectionConfig.SetEPSG(AValue: Integer);
begin
  LockWrite;
  try
    if FEPSG <> AValue then begin
      FEPSG := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
