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

unit u_TileErrorLogProviedrStuped;

interface

uses
  SyncObjs,
  i_JclNotify,
  i_TileError,
  i_TileErrorLogProviedrStuped;

type
  TTileErrorLogProviedrStuped = class(TInterfacedObject, ITileErrorLogProviedrStuped, ITileErrorLogger)
  private
    FLastErrorInfo: ITileErrorInfo;
    FNotifier: IJclNotifier;
    FCS: TCriticalSection;
  protected
    function GetLastErrorInfo: ITileErrorInfo;
    function GetNotifier: IJclNotifier;
  protected
    procedure LogError(AValue: ITileErrorInfo);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_JclNotify;

{ TTileErrorLogProviedrStuped }

constructor TTileErrorLogProviedrStuped.Create;
begin
  FCS := TCriticalSection.Create;
  FNotifier := TJclBaseNotifier.Create;
end;

destructor TTileErrorLogProviedrStuped.Destroy;
begin
  FNotifier:=nil;
  FreeAndNil(FCS);
  inherited;
end;

function TTileErrorLogProviedrStuped.GetLastErrorInfo: ITileErrorInfo;
begin
  FCS.Acquire;
  try
    Result := FLastErrorInfo;
  finally
    FCS.Release;
  end;
end;

function TTileErrorLogProviedrStuped.GetNotifier: IJclNotifier;
begin
  Result := FNotifier;
end;

procedure TTileErrorLogProviedrStuped.LogError(AValue: ITileErrorInfo);
begin
  FCS.Acquire;
  try
    FLastErrorInfo := AValue;
  finally
    FCS.Release;
  end;
  FNotifier.Notify(nil);
end;

end.
