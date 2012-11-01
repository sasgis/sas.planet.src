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

unit u_GoogleEarthTerrainParser;

interface

uses
  t_GeoTypes,
  i_BinaryData;

type
  TGoogleEarthTerrainHandle = Pointer;
  PGoogleEarthTerrainHandle = ^TGoogleEarthTerrainHandle;

  TGoogleEarthTerrainOpen = function(
    const ATerrain: PGoogleEarthTerrainHandle;
    const ATile: Pointer;
    const ASize: Integer;
    const ANeedDecrypt: Boolean
  ): Boolean; cdecl;

  TGoogleEarthTerrainClose = procedure(
    const ATerrain: PGoogleEarthTerrainHandle
  ); cdecl;

  TGoogleEarthTerrainElevation = function(
    const ATerrain: PGoogleEarthTerrainHandle;
    const ALon: Double;
    const ALat: Double;
    const AZoom: Byte;
    out AElevation: Single
  ): Boolean; cdecl;

  TGoogleEarthTerrainParser = class(TObject)
  private
    FAvailable: Boolean;
    FLibHandle: THandle;
    FTerrainOpen: TGoogleEarthTerrainOpen;
    FTerrainClose: TGoogleEarthTerrainClose;
    FGetElevation: TGoogleEarthTerrainElevation;
    function InitLib(const ALibName: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function Open(
      const ADLLContext: PGoogleEarthTerrainHandle;
      const ABinaryData: IBinaryData
    ): Boolean;

    procedure Close(
      const ADLLContext: PGoogleEarthTerrainHandle
    );

    function GetElevation(
      const ADLLContext: PGoogleEarthTerrainHandle;
      const ALonLat: TDoublePoint;
      const AZoom: Byte;
      out AElevation: Single
    ): Boolean;

    property Available: Boolean read FAvailable;
  end;

implementation

uses
  Windows;

const
  cTerrainLibName = 'f1ct.dll';

{ TTerrainParserGoogleEarth }

constructor TGoogleEarthTerrainParser.Create;
begin
  inherited Create;
  FLibHandle := 0;
  FTerrainOpen := nil;
  FTerrainClose := nil;
  FGetElevation := nil;
  FAvailable := InitLib(cTerrainLibName);
end;

destructor TGoogleEarthTerrainParser.Destroy;
begin
  FAvailable := False;
  if FLibHandle > 0 then begin
    FreeLibrary(FLibHandle);
  end;
  FLibHandle := 0;
  FTerrainOpen := nil;
  FTerrainClose := nil;
  FGetElevation := nil;
  inherited Destroy;
end;

function TGoogleEarthTerrainParser.InitLib(const ALibName: string): Boolean;
begin
  FLibHandle := LoadLibrary(PChar(ALibName));
  if FLibHandle > 0 then begin
    FTerrainOpen := GetProcAddress(FLibHandle, 'f1ctOpen');
    FTerrainClose := GetProcAddress(FLibHandle, 'f1ctClose');
    FGetElevation := GetProcAddress(FLibHandle, 'f1ctGetElevation');
  end;
  Result :=
    (FLibHandle > 0) and
    (Addr(FTerrainOpen) <> nil) and
    (Addr(FTerrainClose) <> nil) and
    (Addr(FGetElevation) <> nil);
end;

function TGoogleEarthTerrainParser.Open(
  const ADLLContext: PGoogleEarthTerrainHandle;
  const ABinaryData: IBinaryData
): Boolean;
begin
  Result := False;
  if FAvailable then begin
    Result := FTerrainOpen(
      ADLLContext,
      ABinaryData.Buffer,
      ABinaryData.Size,
      True
    );
  end;
end;

procedure TGoogleEarthTerrainParser.Close(
  const ADLLContext: PGoogleEarthTerrainHandle
);
begin
  if FAvailable then begin
    FTerrainClose(ADLLContext);
  end;
end;

function TGoogleEarthTerrainParser.GetElevation(
  const ADLLContext: PGoogleEarthTerrainHandle;
  const ALonLat: TDoublePoint;
  const AZoom: Byte;
  out AElevation: Single
): Boolean;
begin
  Result := False;
  if FAvailable then begin
    Result := FGetElevation(
      ADLLContext,
      ALonLat.X,
      ALonLat.Y,
      AZoom,
      AElevation
    );
  end;
end;

end.
