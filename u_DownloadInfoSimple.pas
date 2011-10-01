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

unit u_DownloadInfoSimple;

interface

uses
  SyncObjs,
  i_DownloadInfoSimple;

type
  TDownloadInfoSimple = class(TInterfacedObject, IDownloadInfoSimple)
  private
    FParentInfo: IDownloadInfoSimple;
    FCS: TCriticalSection;
    FTileCount: UInt64;
    FSize: UInt64;
  protected
    function GetTileCount: UInt64;
    function GetSize: UInt64;

    procedure Reset;
    procedure Add(ACount: UInt64; ASize: UInt64);
  public
    constructor Create(
      AParent: IDownloadInfoSimple;
      ATileCount: UInt64 = 0;
      ASize: UInt64 = 0
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TDownloadInfoSimple }

constructor TDownloadInfoSimple.Create(
  AParent: IDownloadInfoSimple;
  ATileCount: UInt64;
  ASize: UInt64
);
begin
  inherited Create;
  FParentInfo := AParent;
  FCS := TCriticalSection.Create;
  FTileCount := ATileCount;
  FSize := ASize;
end;

destructor TDownloadInfoSimple.Destroy;
begin
  FreeAndNil(FCS);
  FParentInfo := nil;
  inherited;
end;

procedure TDownloadInfoSimple.Add(ACount, ASize: UInt64);
begin
  FCS.Acquire;
  try
    Inc(FTileCount, ACount);
    Inc(FSize, ASize);
  finally
    FCS.Release;
  end;
  if FParentInfo <> nil then begin
    FParentInfo.Add(ACount, ASize);
  end;
end;

function TDownloadInfoSimple.GetSize: UInt64;
begin
  FCS.Acquire;
  try
    Result := FSize;
  finally
    FCS.Release;
  end;
end;

function TDownloadInfoSimple.GetTileCount: UInt64;
begin
  FCS.Acquire;
  try
    Result := FTileCount;
  finally
    FCS.Release;
  end;
end;

procedure TDownloadInfoSimple.Reset;
begin
  FCS.Acquire;
  try
    FSize := 0;
    FTileCount := 0;
  finally
    FCS.Release;
  end;
end;

end.
