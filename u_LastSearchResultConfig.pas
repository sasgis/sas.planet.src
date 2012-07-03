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

unit u_LastSearchResultConfig;

interface

uses
  i_LastSearchResultConfig,
  i_GeoCoder,
  u_ConfigDataElementBase;

type
  TLastSearchResultConfig = class(TConfigDataElementBaseEmptySaveLoad, ILastSearchResultConfig)
  private
    FIsActive: Boolean;
    FGeoCodeResult: IGeoCodeResult;
  private
    function GetIsActive: Boolean;
    function GetGeoCodeResult: IGeoCodeResult;
    procedure SetGeoCodeResult(const AValue: IGeoCodeResult);
    procedure ClearGeoCodeResult;
  end;

implementation

function TLastSearchResultConfig.GetGeoCodeResult: IGeoCodeResult;
begin
  LockRead;
  try
    Result := FGeoCodeResult;
  finally
    UnlockRead;
  end;
end;

function TLastSearchResultConfig.GetIsActive: Boolean;
begin
  LockRead;
  try
    Result := FIsActive;
  finally
    UnlockRead;
  end;
end;

procedure TLastSearchResultConfig.SetGeoCodeResult(const AValue: IGeoCodeResult);
begin
  LockWrite;
  try
    if FGeoCodeResult <> AValue then begin
      FIsActive := True;
      FGeoCodeResult := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TLastSearchResultConfig.ClearGeoCodeResult;
begin
  LockWrite;
  try
    FIsActive := false;
    FGeoCodeResult := nil;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;


end.
