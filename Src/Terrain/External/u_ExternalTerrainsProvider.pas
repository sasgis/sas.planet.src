{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ExternalTerrainsProvider;

interface

uses
  Windows,
  Types,
  Classes,
  t_GeoTypes,
  i_Notifier,
  i_Listener,
  i_ConfigDataProvider,
  i_ProjConverter,
  i_TerrainConfig,
  i_TerrainProvider,
  u_ExternalTerrainAPI,
  u_BaseInterfacedObject;

type
  TDynamicOptionsReader =
    procedure(
      const ALonLat: TDoublePoint;
      out ALinesCount: Integer;
      out ASamplesCount: Integer
    ) of object;

  TTerrainProviderByExternal = class(TBaseInterfacedObject, ITerrainProvider)
  private
    FUseInterpolation: Boolean;

    FDefaultPath: String;
    FProjConverter: IProjConverter;
    FConfig: ITerrainConfig;
    FConfigListener: IListener;

    // opened file
    FTerrainFile: TTerrainFile;
    FBaseFolder: String;
    // options
    FAvailable: Boolean;
    FSamplesCount: Integer;
    FLinesCount: Integer;
    FVoidValue: Integer;
    FByteOrder: Integer;
    FPrefix, FSuffix: String;
    FLonDigitsWidth, FLatDigitsWidth : Integer;

    FDynamicOptionsReader: TDynamicOptionsReader;

    procedure AlosDynamicOptionsReader(
      const ALonLat: TDoublePoint;
      out ALinesCount: Integer;
      out ASamplesCount: Integer
    );
    procedure OnConfigChange;
  private
    function GetFilenamePart(
      const AValue: Integer;
      const APrefIfPlus, APrefIfMinus: Char;
      const AWidth: Byte
    ): String;
  protected
    { ITerrainProvider }
    function GetPointElevation(
      const ALonLat: TDoublePoint;
      const AZoom: Byte
    ): Single;
    function GetAvailable: Boolean;
    function GetStateChangeNotifier: INotifier;
  public
    constructor Create(
      const AConfig: ITerrainConfig;
      const ADefaultPath: String;
      const AProjConverter: IProjConverter;
      const AOptions: IConfigDataProvider
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  StrUtils,
  SysUtils,
  c_TerrainProvider,
  u_FileSystemFunc,
  u_ListenerByEvent;

{ TTerrainProviderByExternal }

constructor TTerrainProviderByExternal.Create(
  const AConfig: ITerrainConfig;
  const ADefaultPath: String;
  const AProjConverter: IProjConverter;
  const AOptions: IConfigDataProvider
);
begin
  Assert(AConfig <> nil);

  inherited Create;

  FConfig := AConfig;
  FProjConverter := AProjConverter;
  FDefaultPath := IncludeTrailingPathDelimiter(ADefaultPath);

  FUseInterpolation := FConfig.UseInterpolation;
  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfig.ChangeNotifier.Add(FConfigListener);

  // read options
  // if failed - create object but disable it
  FAvailable := AOptions.ReadBool('Enabled', False);

  if (not FAvailable) then begin
    Exit;
  end;

  // folder - terrain file(s) storage
  FBaseFolder := AOptions.ReadString('Folder', '');
  if Length(FBaseFolder) = 0 then begin
    FBaseFolder := FDefaultPath;
  end;

  // get absolute path to storage if it's not
  if StartsText('TerrainData\', FBaseFolder) then begin
    FBaseFolder := StringReplace(FBaseFolder, 'TerrainData\', FDefaultPath, [rfIgnoreCase]);
  end else if StartsText('.\TerrainData\', FBaseFolder) then begin
    FBaseFolder := StringReplace(FBaseFolder, '.\TerrainData\', FDefaultPath, [rfIgnoreCase]);
  end else if StartsText('.', FBaseFolder) then begin
    FBaseFolder := LowerCase(GetFullPath(FDefaultPath, FBaseFolder));
  end else begin
    // it's absolute path
  end;

  if Length(FBaseFolder) = 0 then begin
    FAvailable := FALSE;
    Exit;
  end;

  // samples count in single file (mandatory)
  FSamplesCount := AOptions.ReadInteger('SamplesCount', -1);
  if (FSamplesCount <= 0) then begin
    FAvailable := FALSE;
    Exit;
  end;

  // lines count in single file (check if defined)
  FLinesCount := AOptions.ReadInteger('LinesCount', 0);

  // some optional values
  FVoidValue := AOptions.ReadInteger('VoidValue', cUndefinedElevationValue);

  FByteOrder := AOptions.ReadInteger('ByteOrder', 0);

  FPrefix := Trim(AOptions.ReadString('Prefix', ''));
  FSuffix := Trim(AOptions.ReadString('Suffix', ''));

  FLatDigitsWidth := AOptions.ReadInteger('LatDigitsWidth', 2);
  FLonDigitsWidth := AOptions.ReadInteger('LonDigitsWidth', 3);

  if LowerCase(AOptions.ReadString('DynamicSchema', '')) = 'alos' then begin
    FDynamicOptionsReader := Self.AlosDynamicOptionsReader;
  end else begin
    FDynamicOptionsReader := nil;
  end;

  FTerrainFile := TTerrainFile.Create(FByteOrder, FVoidValue);
end;

destructor TTerrainProviderByExternal.Destroy;
begin
  if (FConfig <> nil) and (FConfigListener <> nil) then begin
    FConfig.ChangeNotifier.Remove(FConfigListener);
    FConfigListener := nil;
  end;
  FreeAndNil(FTerrainFile);
  FProjConverter := nil;
  inherited;
end;

function TTerrainProviderByExternal.GetAvailable: Boolean;
begin
  Result := FAvailable;
end;

function TTerrainProviderByExternal.GetFilenamePart(
  const AValue: Integer;
  const APrefIfPlus, APrefIfMinus: Char;
  const AWidth: Byte
): String;
begin
  // 'N60'
  // 'E056'
  Result := IntToStr(Abs(AValue));

  while Length(Result) < AWidth do begin
    Result := '0' + Result;
  end;

  if AValue < 0 then begin
    Result := APrefIfMinus + Result;
  end else begin
    Result := APrefIfPlus + Result;
  end;
end;

function TTerrainProviderByExternal.GetPointElevation(
  const ALonLat: TDoublePoint;
  const AZoom: Byte
): Single;
var
  VDone: Boolean;
  VFilePoint: TPoint;
  VRow, VCol: Integer;
  VRowCount, VColCount: Integer;
  VFilenameForPoint: string;
  x, y, dx, dy: Single;
  p0, p1, p2, p3: Single;
begin
  Result := cUndefinedElevationValue;

  if FProjConverter <> nil then begin
    // TODO: convert to WGS84/EGM96 geoid
    Exit;
  end;

  if Assigned(FDynamicOptionsReader) then begin
    FDynamicOptionsReader(ALonLat, VRowCount, VColCount);
  end else begin
    VRowCount := FLinesCount;
    VColCount := FSamplesCount;
  end;

  // get filename for given point
  // use common 1x1 distribution (GDEM, STRM, viewfinderpanoramas)
  // TODO: see 'file' implementation for ETOPO1 in ExternalTerrains.dll source
  // TODO: see 'a-p,50' implementation for GLOBE in ExternalTerrains.dll source

  VFilePoint.X := Floor(ALonLat.X);
  VFilePoint.Y := Floor(ALonLat.Y);

  // make filename
  VFilenameForPoint :=
    FBaseFolder +
    FPrefix +
    GetFilenamePart(VFilePoint.Y, 'N', 'S', FLatDigitsWidth) +
    GetFilenamePart(VFilePoint.X, 'E', 'W', FLonDigitsWidth) +
    FSuffix;

  if not FTerrainFile.Open(VFilenameForPoint) then begin
    Exit;
  end;

  if FUseInterpolation then begin
    X := (ALonLat.X - VFilePoint.X) * (VColCount - 1);
    Y := (ALonLat.Y - VFilePoint.Y) * (VRowCount - 1);

    dX := X - Floor(X);
    dY := Y - Floor(Y);

    VCol := Floor((ALonLat.X - VFilePoint.X) * (VColCount - 1));
    VRow := Floor((1 - (ALonLat.Y - VFilePoint.Y)) * (VRowCount - 1));

    VDone :=
      FTerrainFile.FindElevation(VRow, VCol, VRowCount, VColCount, p0) and
      FTerrainFile.FindElevation(VRow, VCol + 1, VRowCount, VColCount, p1) and
      FTerrainFile.FindElevation(VRow - 1, VCol, VRowCount, VColCount, p2) and
      FTerrainFile.FindElevation(VRow - 1, VCol + 1, VRowCount, VColCount, p3);

    // interpolate
    if VDone then begin
      Result :=
        p0 * (1 - dx) * (1 - dy) +
        p1 * dx * (1 - dy) +
        p2 * dy * (1 - dx) +
        p3 * dx * dy;
    end;
  end else begin
    VCol := Round((ALonLat.X - VFilePoint.X) * (VColCount - 1));
    VRow := Round((1 - (ALonLat.Y - VFilePoint.Y)) * (VRowCount - 1));

    if FTerrainFile.FindElevation(VRow, VCol, VRowCount, VColCount, p0) then begin
      Result := p0;
    end;
  end;
end;

function TTerrainProviderByExternal.GetStateChangeNotifier: INotifier;
begin
  Result := nil;
end;

procedure TTerrainProviderByExternal.OnConfigChange;
begin
  FUseInterpolation := FConfig.UseInterpolation;
end;

procedure TTerrainProviderByExternal.AlosDynamicOptionsReader(
  const ALonLat: TDoublePoint;
  out ALinesCount: Integer;
  out ASamplesCount: Integer
);
var
  VLat: Double;
begin
  // https://www.eorc.jaxa.jp/ALOS/en/aw3d30/aw3d30v31_product_e_a.pdf

  ALinesCount := 3600;

  VLat := Abs(ALonLat.Y);

  if VLat < 60 then begin // 0..59
    ASamplesCount := 3600;
  end else
  if VLat < 70 then begin // 60..69
    ASamplesCount := 1800;
  end else
  if VLat < 80 then begin // 70..79
    ASamplesCount := 1200;
  end else
  if VLat <= 90 then begin // 80..90
    ASamplesCount := 600;
  end else begin
    Assert(False);
  end;
end;

end.
