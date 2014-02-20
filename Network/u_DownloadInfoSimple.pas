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

unit u_DownloadInfoSimple;

interface

uses
  SysUtils,
  i_DownloadInfoSimple,
  u_BaseInterfacedObject;

type
  TDownloadInfoSimple = class(TBaseInterfacedObject, IDownloadInfoSimple)
  private
    FParentInfo: IDownloadInfoSimple;
    FCS: IReadWriteSync;
    FTileCount: UInt64;
    FSize: UInt64;
  private
    function GetTileCount: UInt64;
    function GetSize: UInt64;

    procedure Reset;
    procedure Add(
      ACount: UInt64;
      ASize: UInt64
    );
  public
    constructor Create(
      const AParent: IDownloadInfoSimple;
      const ATileCount: UInt64 = 0;
      const ASize: UInt64 = 0
    );
  end;

implementation

uses
  u_Synchronizer;

{ TDownloadInfoSimple }

constructor TDownloadInfoSimple.Create(
  const AParent: IDownloadInfoSimple;
  const ATileCount: UInt64;
  const ASize: UInt64
);
begin
  inherited Create;
  FParentInfo := AParent;
  FCS := MakeSyncRW_Var(Self);
  FTileCount := ATileCount;
  FSize := ASize;
end;

procedure TDownloadInfoSimple.Add(ACount, ASize: UInt64);
begin
  FCS.BeginWrite;
  try
    Inc(FTileCount, ACount);
    Inc(FSize, ASize);
  finally
    FCS.EndWrite;
  end;
  if FParentInfo <> nil then begin
    FParentInfo.Add(ACount, ASize);
  end;
end;

function TDownloadInfoSimple.GetSize: UInt64;
begin
  FCS.BeginRead;
  try
    Result := FSize;
  finally
    FCS.EndRead;
  end;
end;

function TDownloadInfoSimple.GetTileCount: UInt64;
begin
  FCS.BeginRead;
  try
    Result := FTileCount;
  finally
    FCS.EndRead;
  end;
end;

procedure TDownloadInfoSimple.Reset;
begin
  FCS.BeginWrite;
  try
    FSize := 0;
    FTileCount := 0;
  finally
    FCS.EndWrite;
  end;
end;

end.
