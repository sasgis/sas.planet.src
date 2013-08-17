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

unit u_TerrainProviderList;

interface

uses
  SysUtils,
  ActiveX,
  i_GUIDSet,
  i_Notifier,
  i_TerrainProviderList,
  i_TerrainProviderListElement,
  i_PathConfig,
  i_ProjConverter,
  i_GlobalCacheConfig,
  u_BaseInterfacedObject;

type
  TTerrainProviderListBase = class(TBaseInterfacedObject, ITerrainProviderList)
  private
    FList: IGUIDInterfaceSet;
    FCS: IReadWriteSync;
    FAddNotifier: INotifierInternal;
  private
    function GetGUIDEnum: IEnumGUID;
    function Get(const AGUID: TGUID): ITerrainProviderListElement;
    function GetAddNotifier: INotifier;
  protected
    procedure Add(const AItem: ITerrainProviderListElement);
  public
    constructor Create;
  end;

  TTerrainProviderListSimple = class(TTerrainProviderListBase)
  private
    FProjConverterFactory: IProjConverterFactory;
    FTerrainDataPath: IPathConfig;
  private
    procedure LoadFromIni;
  public
    constructor Create(
      const AProjConverterFactory: IProjConverterFactory;
      const ATerrainDataPath: IPathConfig;
      const ACacheConfig: IGlobalCacheConfig
    );
  end;

implementation

uses
  IniFiles,
  Classes,
  c_TerrainProviderGUID,
  u_TerrainProviderListElement,
  u_TerrainProviderByGE,
  u_TerrainProviderByGoogleEarth,
  u_ExternalTerrainsProvider,
  u_Notifier,
  u_Synchronizer,
  u_GUIDInterfaceSet;

{ TTerrainProviderListSimple }

constructor TTerrainProviderListSimple.Create(
  const AProjConverterFactory: IProjConverterFactory;
  const ATerrainDataPath: IPathConfig;
  const ACacheConfig: IGlobalCacheConfig
);
var
  VItem: ITerrainProviderListElement;
begin
  inherited Create;
  FTerrainDataPath := ATerrainDataPath;

  FProjConverterFactory := AProjConverterFactory;

  VItem :=
    TTerrainProviderListElement.Create(
      cTerrainProviderGoogleEarthGUID,
      'GoogleEarth',
      TTerrainProviderByGoogleEarth.Create(ACacheConfig.GECachePath)
    );
  Add(VItem);

  VItem :=
    TTerrainProviderListElement.Create(
      cTerrainProviderGeoCacherGUID,
      'GeoCacher',
      TTerrainProviderByGeoCacher.Create(ACacheConfig)
    );
  Add(VItem);

  // make external items
  LoadFromIni;
end;

procedure TTerrainProviderListSimple.LoadFromIni;
var
  VFileName: String;
  VIniFile: TIniFile;
  VSections, VOptions: TStringList;
  VSection, VCaption: String;
  VGuid: TGUID;
  VItem: ITerrainProviderListElement;
  VProjInitString: AnsiString;
  VProjConverter: IProjConverter;
  i: Integer;
begin
  // check
  if (nil=FTerrainDataPath) then
    Exit;
  VFileName := FTerrainDataPath.FullPath + '\SASTerrain.ini';
  if (not FileExists(VFileName)) then
    Exit;

  // load
  VSections := nil;
  VOptions := nil;
  VIniFile := TIniFile.Create(VFileName);
  try
    VSections := TStringList.Create;
    VOptions := TStringList.Create;
    VIniFile.ReadSections(VSections);
    if (0<VSections.Count) then
    for i := 0 to VSections.Count-1 do
    try
      // loop through terrains
      VSection := VSections[i];

      // get guid
      VCaption := VIniFile.ReadString(VSection, 'GUID', '');
      VGuid := StringToGUID(VCaption);

      // get caption
      VCaption := VIniFile.ReadString(VSection, 'Caption', VSection);

      // get all options
      VIniFile.ReadSectionValues(VSection, VOptions);

      // get proj4 converter
      VProjInitString := VIniFile.ReadString(VSection, 'Proj', '');
      if (0<Length(VProjInitString)) then begin
        VProjConverter := FProjConverterFactory.GetByInitString(VProjInitString);
      end else begin
        // no proj converter
        VProjConverter := nil;
      end;

      // make item
      VItem := TTerrainProviderListElement.Create(
        VGuid,
        VCaption,
        TTerrainProviderByExternal.Create(
          FTerrainDataPath.FullPath,
          VProjConverter,
          VOptions
        )
      );

      // append to list
      Add(VItem);
    except
    end;
  finally
    VIniFile.Free;
    VOptions.Free;
    VSections.Free;
  end;
end;

{ TTerrainProviderListBase }

constructor TTerrainProviderListBase.Create;
begin
  inherited Create;
  FCS := MakeSyncRW_Std(Self, TRUE);
  FList := TGUIDInterfaceSet.Create(False);
  FAddNotifier := TNotifierBase.Create;
end;

procedure TTerrainProviderListBase.Add(const AItem: ITerrainProviderListElement);
begin
  FCS.BeginWrite;
  try
    FList.Add(AItem.GetGUID, AItem);
  finally
    FCS.EndWrite;
  end;
  FAddNotifier.Notify(nil);
end;

function TTerrainProviderListBase.Get(const AGUID: TGUID): ITerrainProviderListElement;
begin
  FCS.BeginRead;
  try
    Result := ITerrainProviderListElement(FList.GetByGUID(AGUID));
  finally
    FCS.EndRead;
  end;
end;

function TTerrainProviderListBase.GetAddNotifier: INotifier;
begin
  Result := FAddNotifier;
end;

function TTerrainProviderListBase.GetGUIDEnum: IEnumGUID;
begin
  FCS.BeginRead;
  try
    Result := FList.GetGUIDEnum;
  finally
    FCS.EndRead;
  end;
end;

end.
