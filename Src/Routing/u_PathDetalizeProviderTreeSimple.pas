{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_PathDetalizeProviderTreeSimple;

interface

uses
  i_PathDetalizeConfig,
  i_StaticTreeItem,
  i_LanguageManager,
  i_InetConfig,
  i_NotifierTime,
  i_DownloaderFactory,
  i_VectorDataFactory,
  i_GeometryLonLatFactory,
  i_VectorDataLoader,
  i_PathDetalizeProviderTreeEntity,
  i_GUIDSet,
  u_TreeChangeableBase;

type
  TProviderGuidList = array of TGUID;

  TPathDetalizeProviderTreeSimple = class(TTreeChangeableBase)
  private
    FZlzkGuidList: TProviderGuidList;
    FProjectOSRMGuidList: TProviderGuidList;
    FOsmScoutGuidList: TProviderGuidList;

    FArrayOfProjectOSRM: TArrayOfProjectOSRM;
    FPathDetalizeConfig: IPathDetalizeConfig;
    FProvidersSet: IGUIDInterfaceSet;

    procedure InitGuidLists;

    function CreateProvidersSet(
      const AInetConfig: IInetConfig;
      const AGCNotifier: INotifierTime;
      const ADownloaderFactory: IDownloaderFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AKmlLoader: IVectorDataLoader
    ): IGUIDInterfaceSet;

    function CreateItem(
      const AName: string;
      const AGroupName: string;
      const AGuidList: TProviderGuidList;
      const ACaptionList: array of string
    ): IStaticTreeItem;
  protected
    function CreateStatic: IStaticTreeItem; override;
  public
    constructor Create(
      const APathDetalizeConfig: IPathDetalizeConfig;
      const ALanguageManager: ILanguageManager;
      const AInetConfig: IInetConfig;
      const AGCNotifier: INotifierTime;
      const ADownloaderFactory: IDownloaderFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AKmlLoader: IVectorDataLoader
    );
  end;

implementation

uses
  SysUtils,
  gnugettext,
  libosmscout_route,
  c_PathDetalizeProvidersGUID,
  i_Downloader,
  i_PathDetalizeProvider,
  i_InterfaceListSimple,
  u_InterfaceListSimple,
  u_StaticTreeItem,
  u_DownloaderHttpWithTTL,
  u_GUIDInterfaceSet,
  u_PathDetalizeProviderTreeEntity,
  u_PathDetalizeProviderOSRM,
  u_PathDetalizeProviderOsmScout;

{ TPathDetalizeProviderTreeSimple }

constructor TPathDetalizeProviderTreeSimple.Create(
  const APathDetalizeConfig: IPathDetalizeConfig;
  const ALanguageManager: ILanguageManager;
  const AInetConfig: IInetConfig;
  const AGCNotifier: INotifierTime;
  const ADownloaderFactory: IDownloaderFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AKmlLoader: IVectorDataLoader
);
begin
  inherited Create(ALanguageManager.ChangeNotifier);
  FPathDetalizeConfig := APathDetalizeConfig;
  FArrayOfProjectOSRM := FPathDetalizeConfig.ArrayOfProjectOSRM;

  InitGuidLists;

  FProvidersSet :=
    CreateProvidersSet(
      AInetConfig,
      AGCNotifier,
      ADownloaderFactory,
      AVectorDataItemMainInfoFactory,
      AVectorGeometryLonLatFactory,
      AKmlLoader
    );
end;

procedure TPathDetalizeProviderTreeSimple.InitGuidLists;
begin
  SetLength(FZlzkGuidList, 3);
  FZlzkGuidList[0] := CPathDetalizeProviderZlzkByCar;
  FZlzkGuidList[1] := CPathDetalizeProviderZlzkByBike;
  FZlzkGuidList[2] := CPathDetalizeProviderZlzkByFoot;

  SetLength(FProjectOsrmGuidList, 3);
  FProjectOsrmGuidList[0] := CPathDetalizeProviderOSRMByCar;
  FProjectOsrmGuidList[1] := CPathDetalizeProviderOSRMByBike;
  FProjectOsrmGuidList[2] := CPathDetalizeProviderOSRMByFoot;

  SetLength(FOsmScoutGuidList, 3);
  FOsmScoutGuidList[0] := CPathDetalizeProviderOsmScoutByCar;
  FOsmScoutGuidList[1] := CPathDetalizeProviderOsmScoutByBike;
  FOsmScoutGuidList[2] := CPathDetalizeProviderOsmScoutByFoot;
end;

procedure AddOsrmProvider(
  const AUrlTemplate: string;
  const AGuidList: TProviderGuidList;
  const AInetConfig: IInetConfig;
  const AGCNotifier: INotifierTime;
  const ADownloaderFactory: IDownloaderFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const ASet: IGUIDInterfaceSet
);
const
  CProfile: array [0..2] of string = ('car', 'bike', 'foot');
var
  I: Integer;
  VBaseUrl: string;
  VDownloader: IDownloader;
  VProvider: IPathDetalizeProvider;
begin
  Assert(Length(AGuidList) = Length(CProfile));
  for I := 0 to Length(CProfile) - 1 do begin
    VBaseUrl := StringReplace(AUrlTemplate, '{profile}', CProfile[I], [rfIgnoreCase]);
    VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, ADownloaderFactory);
    VProvider :=
      TPathDetalizeProviderOSRM.Create(
        AnsiString(VBaseUrl),
        VDownloader,
        AInetConfig,
        AVectorGeometryLonLatFactory
      );
    ASet.Add(AGuidList[I], VProvider);
  end;
end;

procedure AddOsmScoutProvider(
  const AGuidList: TProviderGuidList;
  const AGCNotifier: INotifierTime;
  const APathDetalizeConfig: IPathDetalizeConfig;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const ASet: IGUIDInterfaceSet
);
var
  I: TRouteProfile;
  VProvider: IPathDetalizeProvider;
  VOsmScoutRouteContext: IOsmScoutRouteContext;
begin
  Assert(Length(AGuidList) = 3);

  VOsmScoutRouteContext :=
    NewOsmScoutRouteContext(
      AGCNotifier,
      APathDetalizeConfig,
      ExtractFilePath(ParamStr(0)) + 'osmscout\'
    );

  for I := Low(TRouteProfile) to High(TRouteProfile) do begin
    VProvider :=
      TPathDetalizeProviderOsmScout.Create(
        I,
        VOsmScoutRouteContext,
        AVectorGeometryLonLatFactory
      );
    ASet.Add(AGuidList[Integer(I)], VProvider);
  end;
end;

function TPathDetalizeProviderTreeSimple.CreateProvidersSet(
  const AInetConfig: IInetConfig;
  const AGCNotifier: INotifierTime;
  const ADownloaderFactory: IDownloaderFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AKmlLoader: IVectorDataLoader
): IGUIDInterfaceSet;
var
  I, J: Integer;
  VAddress: string;
  VGuidList: TProviderGuidList;
begin
  Result := TGUIDInterfaceSet.Create;

  if FPathDetalizeConfig.EnableZlzk then begin
    AddOsrmProvider(
      'https://zlzk.biz/route/v1/{profile}/',
      FZlzkGuidList,
      AInetConfig,
      AGCNotifier,
      ADownloaderFactory,
      AVectorGeometryLonLatFactory,
      Result
    );
  end;

  if FPathDetalizeConfig.EnableProjectOSRM then begin
    AddOsrmProvider(
      'https://routing.openstreetmap.de/routed-{profile}/route/v1/driving/',
      FProjectOsrmGuidList,
      AInetConfig,
      AGCNotifier,
      ADownloaderFactory,
      AVectorGeometryLonLatFactory,
      Result
    );
  end;

  for I := 0 to Length(FArrayOfProjectOSRM) - 1 do begin
    VAddress := FArrayOfProjectOSRM[I].Address;
    if VAddress = '' then begin
      Assert(False);
      Continue;
    end;
    if Pos('://', VAddress) <= 0 then begin
      VAddress := 'http://' + VAddress;
    end;
    if VAddress[Length(VAddress)] <> '/' then begin
      VAddress := VAddress + '/';
    end;

    SetLength(VGuidList, 3);
    for J := 0 to Length(VGuidList) - 1 do begin
      VGuidList[J] := FArrayOfProjectOSRM[I].Guid[J];
    end;

    AddOsrmProvider(
      VAddress + 'route/v1/{profile}/',
      VGuidList,
      AInetConfig,
      AGCNotifier,
      ADownloaderFactory,
      AVectorGeometryLonLatFactory,
      Result
    );
  end;

  if IsLibOsmScoutRouteAvailable then begin
    AddOsmScoutProvider(
      FOsmScoutGuidList,
      AGCNotifier,
      FPathDetalizeConfig,
      AVectorGeometryLonLatFactory,
      Result
    );
  end;
end;

function TPathDetalizeProviderTreeSimple.CreateItem(
  const AName: string;
  const AGroupName: string;
  const AGuidList: TProviderGuidList;
  const ACaptionList: array of string
): IStaticTreeItem;
var
  I: Integer;
  VList: IInterfaceListSimple;
  VProvider: IPathDetalizeProvider;
  VEntity: IPathDetalizeProviderTreeEntity;
  VItem: IStaticTreeItem;
begin
  VList := TInterfaceListSimple.Create;

  for I := 0 to Length(AGuidList) - 1 do begin
    VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(AGuidList[I]));
    VEntity :=
      TPathDetalizeProviderTreeEntity.Create(
        AGuidList[I],
        '',
        ACaptionList[I],
        VProvider
      );
    VItem :=
      TStaticTreeItem.Create(
        VEntity,
        VEntity.MenuItemName,
        Format('%.4d0', [I]),
        nil
      );
    VList.Add(VItem);
  end;

  Result :=
    TStaticTreeItem.Create(
      nil,
      AName,
      AGroupName,
      VList.MakeStaticAndClear
    );
end;

function TPathDetalizeProviderTreeSimple.CreateStatic: IStaticTreeItem;

  function _GetOsrmCaption(const AItemId: Integer): string;
  var
    I: Integer;
  begin
    Result := FArrayOfProjectOSRM[AItemId].Address;
    I := Pos('://', Result);
    if I > 0 then begin
      Result := Copy(Result, I+3);
    end;
    I := Length(Result);
    if (I > 0) and (Result[I] = '/') then begin
      SetLength(Result, I-1);
    end;
    Result := Result + ' (OSRM)';
  end;

  function _GetOsrmGuidList(const AItemId: Integer): TProviderGuidList;
  var
    I: Integer;
  begin
    SetLength(Result, 3);
    for I := 0 to Length(Result) - 1 do begin
      Result[I] := FArrayOfProjectOSRM[AItemId].Guid[I];
    end;
  end;

var
  I: Integer;
  VGroupId: Integer;
  VItem: IStaticTreeItem;
  VList: IInterfaceListSimple;
begin
  VGroupId := 0;
  VList := TInterfaceListSimple.Create;

  if FPathDetalizeConfig.EnableZlzk then begin
    Inc(VGroupId);
    VItem :=
      CreateItem(
        'zlzk.biz (OSRM)',
        Format('%.4d0~', [VGroupId]),
        FZlzkGuidList,
        [ _('By Car'), _('By Bike'), _('By Foot') ]
      );
    VList.Add(VItem);
  end;

  if FPathDetalizeConfig.EnableProjectOSRM then begin
    Inc(VGroupId);
    VItem :=
      CreateItem(
        'Project OSRM',
        Format('%.4d0~', [VGroupId]),
        FProjectOSRMGuidList,
        [ _('By Car'), _('By Bike'), _('By Foot') ]
      );
    VList.Add(VItem);
  end;

  for I := 0 to Length(FArrayOfProjectOSRM) - 1 do begin
    Inc(VGroupId);
    VItem :=
      CreateItem(
        _GetOsrmCaption(I),
        Format('%.4d0~', [VGroupId]),
        _GetOsrmGuidList(I),
        [ _('By Car'), _('By Bike'), _('By Foot') ]
      );
    VList.Add(VItem);
  end;

  if IsLibOsmScoutRouteAvailable then begin
    Inc(VGroupId);
    VItem :=
      CreateItem(
        'OSM Scout (offline)',
        Format('%.4d0~', [VGroupId]),
        FOsmScoutGuidList,
        [ _('By Car'), _('By Bike'), _('By Foot') ]
      );
    VList.Add(VItem);
  end;

  Result :=
    TStaticTreeItem.Create(
      nil,
      '',
      '',
      VList.MakeStaticAndClear
    );
end;

end.
