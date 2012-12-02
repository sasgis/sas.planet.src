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

unit u_CacheConverterProgressInfo;

interface

uses
  SysUtils,
  i_CacheConverterProgressInfo,
  u_BaseInterfacedObject;

type
  TCacheConverterProgressInfo = class(TBaseInterfacedObject, ICacheConverterProgressInfo)
  private
    FTilesProcessed: Int64;
    FTilesSkipped: Int64;
    FTilesSize: Int64;
    FLastTileName: string;
    FCS: IReadWriteSync;
    FFinished: Boolean;
    FProgressAbortErrorStr: string;
  private
    function GetTilesProcessed: Int64;
    procedure SetTilesProcessed(const AValue: Int64);
    function GetTilesSkipped: Int64;
    procedure SetTilesSkipped(const AValue: Int64);
    function GetTilesSize: Int64;
    procedure SetTilesSize(const AValue: Int64);
    function GetLastTileName: string;
    procedure SetLastTileName(const AValue: string);
    function GetIsFinished: Boolean;
    procedure SetIsFinished(const AValue: Boolean);
    function GetProgressAbortErrorStr: string;
    procedure SetProgressAbortErrorStr(const AValue: string);
  public
    constructor Create;
  end;

implementation

uses
  u_Synchronizer;

{ TCacheConverterProgressInfo }

constructor TCacheConverterProgressInfo.Create;
begin
  inherited Create;
  FCS := MakeSyncRW_Var(Self, False);
  FTilesProcessed := 0;
  FTilesSkipped := 0;
  FTilesSize := 0;
  FLastTileName := '';
  FFinished := False;
  FProgressAbortErrorStr := '';
end;

function TCacheConverterProgressInfo.GetTilesProcessed: Int64;
begin
  FCS.BeginRead;
  try
    Result := FTilesProcessed;
  finally
    FCS.EndRead
  end;
end;

procedure TCacheConverterProgressInfo.SetTilesProcessed(const AValue: Int64);
begin
  FCS.BeginWrite;
  try
    FTilesProcessed := AValue;
  finally
    FCS.EndWrite;
  end;
end;

function TCacheConverterProgressInfo.GetTilesSkipped: Int64;
begin
  FCS.BeginRead;
  try
    Result := FTilesSkipped;
  finally
    FCS.EndRead;
  end;
end;

procedure TCacheConverterProgressInfo.SetTilesSkipped(const AValue: Int64);
begin
  FCS.BeginWrite;
  try
    FTilesSkipped := AValue;
  finally
    FCS.EndWrite;
  end;
end;

function TCacheConverterProgressInfo.GetTilesSize: Int64;
begin
  FCS.BeginRead;
  try
    Result := FTilesSize;
  finally
    FCS.EndRead;
  end;
end;

procedure TCacheConverterProgressInfo.SetTilesSize(const AValue: Int64);
begin
  FCS.BeginWrite;
  try
    FTilesSize := AValue;
  finally
    FCS.EndWrite;
  end;
end;

function TCacheConverterProgressInfo.GetLastTileName: string;
begin
  FCS.BeginRead;
  try
    Result := FLastTileName;
  finally
    FCS.EndRead;
  end;
end;

procedure TCacheConverterProgressInfo.SetLastTileName(const AValue: string);
begin
  FCS.BeginWrite;
  try
    FLastTileName := AValue;
  finally
    FCS.EndWrite;
  end;
end;

function TCacheConverterProgressInfo.GetIsFinished: Boolean;
begin
  FCS.BeginRead;
  try
    Result := FFinished;
  finally
    FCS.EndRead;
  end;
end;

procedure TCacheConverterProgressInfo.SetIsFinished(const AValue: Boolean);
begin
  FCS.BeginWrite;
  try
    FFinished := AValue;
  finally
    FCS.EndWrite
  end;
end;

function TCacheConverterProgressInfo.GetProgressAbortErrorStr: string;
begin
  FCS.BeginRead;
  try
    Result := FProgressAbortErrorStr;
  finally
    FCS.EndRead;
  end;
end;

procedure TCacheConverterProgressInfo.SetProgressAbortErrorStr(const AValue: string);
begin
  FCS.BeginWrite;
  try
    FProgressAbortErrorStr := AValue;
  finally
    FCS.EndWrite
  end;
end;

end.
