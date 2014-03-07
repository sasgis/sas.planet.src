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

unit u_TileDownloaderListStatic;

interface

uses
  i_TileDownloader,
  i_TileDownloaderList,
  u_BaseInterfacedObject;

type
  TTileDownloaderListStatic = class(TBaseInterfacedObject, ITileDownloaderListStatic)
  private
    FList: array of ITileDownloader;
    FCount: Integer;
  private
    function GetCount: Integer;
    function GetItem(AIndex: Integer): ITileDownloader;
  public
    constructor Create(
      const AList: array of ITileDownloader
    );
    destructor Destroy; override;
  end;

implementation

{ TTileDownloaderListStatic }

constructor TTileDownloaderListStatic.Create(
  const AList: array of ITileDownloader
);
var
  i: Integer;
begin
  inherited Create;
  FCount := Length(AList);
  SetLength(FList, FCount);
  for i := 0 to FCount - 1 do begin
    FList[i] := AList[i];
  end;
end;

destructor TTileDownloaderListStatic.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do begin
    FList[i] := nil;
  end;
  FCount := 0;
  FList := nil;
  inherited;
end;

function TTileDownloaderListStatic.GetCount: Integer;
begin
  Result := FCount;
end;

function TTileDownloaderListStatic.GetItem(AIndex: Integer): ITileDownloader;
begin
  Result := FList[AIndex];
end;

end.
