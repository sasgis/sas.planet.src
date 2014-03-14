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

unit u_MainMemCacheConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MainMemCacheConfig,
  u_ConfigDataElementBase;

type
  TMainMemCacheConfig = class(TConfigDataElementBase, IMainMemCacheConfig)
  private
    FMaxSize: Integer;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetMaxSize: Integer;
    procedure SetMaxSize(AValue: Integer);
  public
    constructor Create;
  end;

implementation

const
  CMaxValidSize = 400;

{ TMainMemCacheConfig }

constructor TMainMemCacheConfig.Create;
begin
  inherited Create;
  FMaxSize := 100;
end;

procedure TMainMemCacheConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetMaxSize(AConfigData.ReadInteger('UICachePerZmp', FMaxSize));
  end;
end;

procedure TMainMemCacheConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteInteger('UICachePerZmp', FMaxSize);
end;

function TMainMemCacheConfig.GetMaxSize: Integer;
begin
  LockRead;
  try
    Result := FMaxSize;
  finally
    UnlockRead;
  end;
end;

procedure TMainMemCacheConfig.SetMaxSize(AValue: Integer);
var
  VMaxSize: Integer;
begin
  VMaxSize := AValue;
  if VMaxSize < 0 then begin
    VMaxSize := 0;
  end else if VMaxSize > CMaxValidSize then begin
    VMaxSize := CMaxValidSize;
  end;

  LockWrite;
  try
    if FMaxSize <> VMaxSize then begin
      FMaxSize := VMaxSize;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
