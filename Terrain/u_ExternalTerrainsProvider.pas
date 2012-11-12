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

unit u_ExternalTerrainsProvider;

interface

uses
  Windows,
  Types,
  t_GeoTypes,
  t_ExternalTerrainAPI,
  i_Notifier,
  i_ExternalTerrainsProvider,
  i_ProjConverter,
  i_TerrainProvider;

type
  TExternalTerrainsProvider = class(TInterfacedObject, IExternalTerrainsProvider, IExternalTerrainsProviderInternal)
  private
    FDLLHandle: THandle;
    FLibAvail: Boolean;
    FEnumFunc: Pointer;
    FOpenFunc: Pointer;
    FCloseFunc: Pointer;
    FElevFunc: Pointer;
  private
    { IExternalTerrainsProvider }
    function Available: Boolean;
    function Enum(
      const AHostPointer: Pointer;
      const AHostCallback: TExternalTerrainsEnumCallback
    ): Boolean;
  private
    { IExternalTerrainsProviderInternal }
    function OpenFunc: Pointer;
    function CloseFunc: Pointer;
    function ElevFunc: Pointer;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function CreateProvider(
      const AProjConverter: IProjConverter;
      const AProviderGUID: TGUID
    ): ITerrainProvider;
  end;

implementation

uses
  c_TerrainProvider,
  SysUtils;

type
  TTerrainProviderByExternal = class(TInterfacedObject, ITerrainProvider)
  private
    FExternalTerrainsProviderInternal: IExternalTerrainsProviderInternal;
    FProjConverter: IProjConverter;
    FProviderGUID: TGUID;
  private
    FProvHandle: TExternalTerrainsHandle;
  protected
    { ITerrainProvider }
    function GetPointElevation(const ALonLat: TDoublePoint; const AZoom: Byte): Single;
    function GetAvailable: Boolean;
    function GetStateChangeNotifier: INotifier;
  public
    constructor Create(
      const AExternalTerrainsProviderInternal: IExternalTerrainsProviderInternal;
      const AProjConverter: IProjConverter;
      const AProviderGUID: TGUID
    );
    destructor Destroy; override;
  end;

{ TExternalTerrainsProvider }

function TExternalTerrainsProvider.Available: Boolean;
begin
  Result := FLibAvail;
end;

function TExternalTerrainsProvider.CloseFunc: Pointer;
begin
  Result := FCloseFunc;
end;

constructor TExternalTerrainsProvider.Create;
begin
  inherited Create;
  FDLLHandle := LoadLibrary('ExternalTerrains.dll');
  if (FDLLHandle<>0) then begin
    // ok
    FEnumFunc  := GetProcAddress(FDLLHandle, 'ExternalTerrainsEnum');
    FOpenFunc  := GetProcAddress(FDLLHandle, 'ExternalTerrainsOpen');
    FCloseFunc := GetProcAddress(FDLLHandle, 'ExternalTerrainsClose');
    FElevFunc  := GetProcAddress(FDLLHandle, 'ExternalTerrainsGetElevation');
    // check available
    FLibAvail := (FEnumFunc<>nil) and (FOpenFunc<>nil) and (FCloseFunc<>nil) and (FElevFunc<>nil);
  end else begin
    // failed
    FLibAvail := FALSE;
  end;
end;

function TExternalTerrainsProvider.CreateProvider(
  const AProjConverter: IProjConverter;
  const AProviderGUID: TGUID
): ITerrainProvider;
begin
  Result := TTerrainProviderByExternal.Create(
    Self,
    AProjConverter,
    AProviderGUID
  );
end;

destructor TExternalTerrainsProvider.Destroy;
begin
  if (FDLLHandle<>0) then begin
    FreeLibrary(FDLLHandle);
    FDLLHandle:=0;
  end;
  inherited;
end;

function TExternalTerrainsProvider.ElevFunc: Pointer;
begin
  Result := FElevFunc;
end;

function TExternalTerrainsProvider.Enum(const AHostPointer: Pointer; const AHostCallback: TExternalTerrainsEnumCallback): Boolean;
begin
  Result := TExternalTerrainsEnum(FEnumFunc)(AHostPointer, Pointer(Self), AHostCallback);
end;

function TExternalTerrainsProvider.OpenFunc: Pointer;
begin
  Result := FOpenFunc;
end;

{ TTerrainProviderByExternal }

constructor TTerrainProviderByExternal.Create(
  const AExternalTerrainsProviderInternal: IExternalTerrainsProviderInternal;
  const AProjConverter: IProjConverter;
  const AProviderGUID: TGUID
);
begin
  inherited Create;
  FExternalTerrainsProviderInternal := AExternalTerrainsProviderInternal;
  FProjConverter := AProjConverter;
  FProviderGUID := AProviderGUID;
  FProvHandle := nil;

  // open
  TExternalTerrainsOpen(FExternalTerrainsProviderInternal.OpenFunc)(@FProvHandle, nil, AProviderGUID, (FProjConverter<>nil));
end;

destructor TTerrainProviderByExternal.Destroy;
begin
  if (FProvHandle <> nil) then begin
    TExternalTerrainsClose(FExternalTerrainsProviderInternal.CloseFunc)(@FProvHandle);
    FProvHandle := nil;
  end;
  FProjConverter := nil;
  FExternalTerrainsProviderInternal := nil;
  inherited;
end;

function TTerrainProviderByExternal.GetAvailable: Boolean;
begin
  Result := (FProvHandle<>nil) and FExternalTerrainsProviderInternal.Available;
end;

function TTerrainProviderByExternal.GetPointElevation(
  const ALonLat: TDoublePoint;
  const AZoom: Byte
): Single;
begin
  if (FProjConverter<>nil) then begin
    // TODO: convert to WGS84/EGM96 geoid
    Result := cUndefinedElevationValue;
  end else begin
    // without conversion
    if not TExternalTerrainsGetElevation(FExternalTerrainsProviderInternal.ElevFunc)(@FProvHandle, ALonLat.X, ALonLat.Y, Result) then
      Result := cUndefinedElevationValue;
  end;
end;

function TTerrainProviderByExternal.GetStateChangeNotifier: INotifier;
begin
  Result := nil;
end;

end.
