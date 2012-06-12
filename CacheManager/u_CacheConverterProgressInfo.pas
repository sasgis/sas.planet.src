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
  SyncObjs,
  i_CacheConverterProgressInfo;

type
  TCacheConverterProgressInfo = class(TInterfacedObject, ICacheConverterProgressInfo)
  private
    FTilesProcessed: Int64;
    FTilesSkipped: Int64;
    FTilesSize: Int64;
    FLastTileName: string;
    FCS: TCriticalSection;
    FFinished: Boolean;
  protected
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
  public
    constructor Create;
    destructor Destroy; override;
    property TilesProcessed: Int64 read GetTilesProcessed write SetTilesProcessed;
    property TilesSkipped: Int64 read GetTilesSkipped write SetTilesSkipped;
    property TilesSize: Int64 read GetTilesSize write SetTilesSize;
    property LastTileName: string read GetLastTileName write SetLastTileName;
    property Finished: Boolean read GetIsFinished write SetIsFinished;
  end;

implementation

{ TCacheConverterProgressInfo }

constructor TCacheConverterProgressInfo.Create;
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FTilesProcessed := 0;
  TilesSkipped := 0;
  FTilesSize := 0;
  FLastTileName := '';
  FFinished := False;
end;

destructor TCacheConverterProgressInfo.Destroy;
begin
  FCS.Free;
  inherited Destroy;
end;

function TCacheConverterProgressInfo.GetTilesProcessed: Int64;
begin
  FCS.Acquire;
  try
    Result := FTilesProcessed;
  finally
    FCS.Release
  end;
end;

procedure TCacheConverterProgressInfo.SetTilesProcessed(const AValue: Int64);
begin
  FCS.Acquire;
  try
    FTilesProcessed := AValue;
  finally
    FCS.Release
  end;
end;

function TCacheConverterProgressInfo.GetTilesSkipped: Int64;
begin
  FCS.Acquire;
  try
    Result := FTilesSkipped;
  finally
    FCS.Release
  end;
end;

procedure TCacheConverterProgressInfo.SetTilesSkipped(const AValue: Int64);
begin
  FCS.Acquire;
  try
    FTilesSkipped := AValue;
  finally
    FCS.Release
  end;
end;

function TCacheConverterProgressInfo.GetTilesSize: Int64;
begin
  FCS.Acquire;
  try
    Result := FTilesSize;
  finally
    FCS.Release
  end;
end;

procedure TCacheConverterProgressInfo.SetTilesSize(const AValue: Int64);
begin
  FCS.Acquire;
  try
    FTilesSize := AValue;
  finally
    FCS.Release
  end;
end;

function TCacheConverterProgressInfo.GetLastTileName: string;
begin
  FCS.Acquire;
  try
    Result := FLastTileName;
  finally
    FCS.Release
  end;
end;

procedure TCacheConverterProgressInfo.SetLastTileName(const AValue: string);
begin
  FCS.Acquire;
  try
    FLastTileName := AValue;
  finally
    FCS.Release
  end;
end;

function TCacheConverterProgressInfo.GetIsFinished: Boolean;
begin
  FCS.Acquire;
  try
    Result := FFinished;
  finally
    FCS.Release
  end;
end;

procedure TCacheConverterProgressInfo.SetIsFinished(const AValue: Boolean);
begin
  FCS.Acquire;
  try
    FFinished := AValue;
  finally
    FCS.Release
  end;
end;

end.
