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
  i_ExternalTerrainsProvider,
  i_ProjConverter,
  u_GlobalCahceConfig;

type
  TTerrainProviderListBase = class(TInterfacedObject, ITerrainProviderList)
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
    destructor Destroy; override;
  end;

  TTerrainProviderListSimple = class(TTerrainProviderListBase)
  private
    FProjConverterFactory: IProjConverterFactory;
    FExternalTerrainsProvider: IExternalTerrainsProvider;
  public
    constructor Create(
      const AProjConverterFactory: IProjConverterFactory;
      const ACacheConfig: TGlobalCahceConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  c_TerrainProviderGUID,
  u_TerrainProviderListElement,
  u_TerrainProviderByGE,
  u_ExternalTerrainsProvider,
  u_Notifier,
  u_Synchronizer,
  u_GUIDInterfaceSet;

function ExternalTerrainsEnumCallbackFunc(
  const AHostPointer: Pointer; // list
  const ACallPointer: Pointer; // enumerator
  const AProviderGUID: TGUID;
  const AProviderName: PWideChar;
  const AProviderProj: PAnsiChar;
  const AOptions: LongWord // reserved
): Boolean; cdecl;
var
  VCaption: WideString;
  VProjInitString: AnsiString;
  VProjConverter: IProjConverter;
  VItem: ITerrainProviderListElement;
begin
  try
    VCaption := AProviderName;

    if (AProviderProj<>nil) then begin
      VProjInitString := AProviderProj;
      VProjConverter := TTerrainProviderListSimple(AHostPointer).FProjConverterFactory.GetByInitString(VProjInitString);
    end else begin
      // no proj converter
      VProjConverter := nil;
    end;

    VItem := TTerrainProviderListElement.Create(
      AProviderGUID,
      VCaption,
      TExternalTerrainsProvider(ACallPointer).CreateProvider(VProjConverter, AProviderGUID)
    );

    TTerrainProviderListSimple(AHostPointer).Add(VItem);

    Result := TRUE;
  except
    Result := FALSE;
  end;
end;

{ TTerrainProviderListSimple }

constructor TTerrainProviderListSimple.Create(
  const AProjConverterFactory: IProjConverterFactory;
  const ACacheConfig: TGlobalCahceConfig
);
var
  VItem: ITerrainProviderListElement;
begin
  inherited Create;

  FProjConverterFactory := AProjConverterFactory;

  VItem :=
    TTerrainProviderListElement.Create(
      cTerrainProviderGoogleEarthGUID,
      'GoogleEarth',
      TTerrainProviderByGoogleEarth.Create(ACacheConfig)
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
  FExternalTerrainsProvider := TExternalTerrainsProvider.Create;
  if FExternalTerrainsProvider.Available then begin
    FExternalTerrainsProvider.Enum(Pointer(Self), @ExternalTerrainsEnumCallbackFunc);
  end;
end;

destructor TTerrainProviderListSimple.Destroy;
begin
  FExternalTerrainsProvider := nil;
  FProjConverterFactory := nil;
  inherited;
end;

{ TTerrainProviderListBase }

constructor TTerrainProviderListBase.Create;
begin
  inherited Create;
  FCS := MakeSyncRW_Std(Self, TRUE);
  FList := TGUIDInterfaceSet.Create(False);
  FAddNotifier := TNotifierBase.Create;
end;

destructor TTerrainProviderListBase.Destroy;
begin
  FList := nil;
  FCS := nil;
  inherited;
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
