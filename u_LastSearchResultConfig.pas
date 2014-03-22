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

unit u_LastSearchResultConfig;

interface

uses
  i_LastSearchResultConfig,
  i_GeoCoder,
  u_ConfigDataElementBase;

type
  TLastSearchResult = class(TConfigDataElementBaseEmptySaveLoad, ILastSearchResult)
  private
    FGeoCodeResult: IGeoCodeResult;
  private
    function GetGeoCodeResult: IGeoCodeResult;
    procedure SetGeoCodeResult(const AValue: IGeoCodeResult);
    procedure ClearGeoCodeResult;
  end;

implementation

function TLastSearchResult.GetGeoCodeResult: IGeoCodeResult;
begin
  LockRead;
  try
    Result := FGeoCodeResult;
  finally
    UnlockRead;
  end;
end;

procedure TLastSearchResult.SetGeoCodeResult(const AValue: IGeoCodeResult);
begin
  LockWrite;
  try
    if FGeoCodeResult <> AValue then begin
      FGeoCodeResult := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TLastSearchResult.ClearGeoCodeResult;
begin
  LockWrite;
  try
    FGeoCodeResult := nil;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;


end.
