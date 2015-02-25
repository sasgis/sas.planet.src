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

unit u_LastSearchResult;

interface

uses
  SysUtils,
  i_LastSearchResult,
  i_GeoCoder,
  u_ChangeableBase;

type
  TLastSearchResult = class(TChangeableBase, ILastSearchResult)
  private
    FGeoCodeResult: IGeoCodeResult;
    FCS: IReadWriteSync;
  private
    function GetGeoCodeResult: IGeoCodeResult;
    procedure SetGeoCodeResult(const AValue: IGeoCodeResult);
    procedure ClearGeoCodeResult;
  public
    constructor Create;
  end;

implementation

uses
  u_Synchronizer;

constructor TLastSearchResult.Create;
var
  VCS: IReadWriteSync;
begin
  VCS := GSync.SyncVariable.Make(ClassName);
  inherited Create(VCS);
  FCS := VCS;
end;

function TLastSearchResult.GetGeoCodeResult: IGeoCodeResult;
begin
  FCS.BeginRead;
  try
    Result := FGeoCodeResult;
  finally
    FCS.EndRead;
  end;
end;

procedure TLastSearchResult.SetGeoCodeResult(const AValue: IGeoCodeResult);
var
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  FCS.BeginWrite;
  try
    if FGeoCodeResult <> AValue then begin
      FGeoCodeResult := AValue;
      VNeedNotify := True;
    end;
  finally
    FCS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

procedure TLastSearchResult.ClearGeoCodeResult;
var
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  FCS.BeginWrite;
  try
    if Assigned(FGeoCodeResult) then begin
      FGeoCodeResult := nil;
      VNeedNotify := True;
    end;
  finally
    FCS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;


end.
