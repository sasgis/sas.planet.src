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

unit u_VectorItemSubsetChangeableForVectorLayers;

interface

uses
  SysUtils,
  t_GeoTypes,
  i_VectorItemSubsetBuilder,
  i_VectorItemSubset,
  i_VectorItemSubsetChangeable,
  i_Listener,
  i_InterfaceListSimple,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_ThreadConfig,
  i_BackgroundTask,
  i_MapVersionRequest,
  i_MapTypes,
  i_MapTypeSet,
  i_MapTypeSetChangeable,
  i_SimpleFlag,
  i_NotifierOperation,
  i_VectorDataItemSimple,
  i_InterfaceListStatic,
  i_InternalPerformanceCounter,
  i_ListenerNotifierLinksList,
  i_TileError,
  u_ChangeableBase;

type
  TVectorItemSubsetChangeableForVectorLayers = class(TChangeableBase, IVectorItemSubsetChangeable)
  private
    FLayersSet: IMapTypeSetChangeable;
    FErrorLogger: ITileErrorLogger;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FPosition: ILocalCoordConverterChangeable;
    FAppStartedNotifier: INotifierOneOperation;
    FAppClosingNotifier: INotifierOneOperation;

    FAppStartedListener: IListener;
    FAppClosingListener: IListener;

    FPrepareResultTask: IBackgroundTask;
    FLinksList: IListenerNotifierLinksList;
    FSubsetPrepareCounter: IInternalPerformanceCounter;
    FOneTilePrepareCounter: IInternalPerformanceCounter;

    FPrevLayerSet: IMapTypeSet;
    FPrevLocalConverter: ILocalCoordConverter;
    FLayerListeners: IInterfaceListStatic;
    FVersionListener: IListener;

    FDelicateUpdateFlag: ISimpleFlag;

    FResultCS: IReadWriteSync;
    FResult: IVectorItemSubset;


    procedure OnAppStarted;
    procedure OnAppClosing;

    procedure OnPosChange;
    procedure OnLayerSetChange;

    procedure OnMapVersionChange;
    procedure OnTileUpdate(const AMsg: IInterface);
    procedure OnPrepareSubset(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
    function PrepareSubset(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter;
      const ALayerSet: IMapTypeSet
    ): IVectorItemSubset;
    procedure AddWikiElement(
      const AElments: IVectorItemSubsetBuilder;
      const AData: IVectorDataItemSimple;
      const ALocalConverter: ILocalCoordConverter
    );
    procedure AddElementsFromMap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AElments: IVectorItemSubsetBuilder;
      const AAlayer: IMapType;
      const AVersion: IMapVersionRequest;
      const ALocalConverter: ILocalCoordConverter
    );

    procedure RemoveLayerListeners(
      const ALayerSet: IMapTypeSet
    );
    procedure AddLayerListeners(
      const ALocalConverter: ILocalCoordConverter;
      const ALayerSet: IMapTypeSet
    );
    procedure RemoveVersionListener(
      const ALayerSet: IMapTypeSet
    );
    procedure AddVersionListener(
      const ALayerSet: IMapTypeSet
    );
  private
    function GetStatic: IVectorItemSubset;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const APosition: ILocalCoordConverterChangeable;
      const ALayersSet: IMapTypeSetChangeable;
      const AErrorLogger: ITileErrorLogger;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AThreadConfig: IThreadConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  ActiveX,
  i_NotifierTilePyramidUpdate,
  i_CoordConverter,
  i_TileIterator,
  i_LonLatRect,
  i_GeometryLonLat,
  u_InterfaceListSimple,
  u_SimpleFlagWithInterlock,
  u_ListenerNotifierLinksList,
  u_BackgroundTask,
  u_TileUpdateListenerToLonLat,
  u_ListenerByEvent,
  u_TileIteratorByRect,
  u_TileErrorInfo,
  u_ResStrings,
  u_Synchronizer,
  u_GeoFunc;

{ TVectorItemSubsetChangeableForVectorLayers }

constructor TVectorItemSubsetChangeableForVectorLayers.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const APosition: ILocalCoordConverterChangeable;
  const ALayersSet: IMapTypeSetChangeable;
  const AErrorLogger: ITileErrorLogger;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AThreadConfig: IThreadConfig
);
begin
  Assert(Assigned(APosition));
  Assert(Assigned(ALayersSet));
  Assert(Assigned(AErrorLogger));
  Assert(Assigned(AVectorItemSubsetBuilderFactory));
  inherited Create();
  FPosition := APosition;
  FLayersSet := ALayersSet;
  FErrorLogger := AErrorLogger;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;

  FSubsetPrepareCounter := APerfList.CreateAndAddNewCounter('SubsetPrepare');
  FOneTilePrepareCounter := APerfList.CreateAndAddNewCounter('OneTilePrepare');

  FAppStartedNotifier := AAppStartedNotifier;
  FAppClosingNotifier := AAppClosingNotifier;

  FDelicateUpdateFlag := TSimpleFlagWithInterlock.Create;
  FResultCS := GSync.SyncVariable.Make(Self.ClassName);
  FLinksList := TListenerNotifierLinksList.Create;
  FAppStartedListener := TNotifyNoMmgEventListener.Create(Self.OnAppStarted);
  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);

  FVersionListener := TNotifyNoMmgEventListener.Create(Self.OnMapVersionChange);

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FPosition.ChangeNotifier
  );

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLayerSetChange),
    FLayersSet.ChangeNotifier
  );
  FPrepareResultTask :=
    TBackgroundTask.Create(
      AAppClosingNotifier,
      OnPrepareSubset,
      AThreadConfig,
      Self.ClassName
    );

end;

destructor TVectorItemSubsetChangeableForVectorLayers.Destroy;
begin
  RemoveLayerListeners(FPrevLayerSet);
  RemoveVersionListener(FPrevLayerSet);

  FLinksList := nil;

  if Assigned(FAppStartedNotifier) and Assigned(FAppStartedListener) then begin
    FAppStartedNotifier.Remove(FAppStartedListener);
    FAppStartedNotifier := nil;
  end;
  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingNotifier := nil;
  end;

  inherited;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.AfterConstruction;
begin
  inherited;
  FAppStartedNotifier.Add(FAppStartedListener);
  if FAppStartedNotifier.IsExecuted then begin
    OnAppStarted;
  end;
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnAppClosing;
  end;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.BeforeDestruction;
begin
  inherited;
  OnAppClosing;
end;

function TVectorItemSubsetChangeableForVectorLayers.GetStatic: IVectorItemSubset;
begin
  FResultCS.BeginRead;
  try
    Result := FResult;
  finally
    FResultCS.EndRead;
  end;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.OnAppClosing;
begin
  FLinksList.DeactivateLinks;
  FPrepareResultTask.Terminate;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.OnAppStarted;
begin
  FLinksList.ActivateLinks;
  FPrepareResultTask.Start;
  FPrepareResultTask.StartExecute;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.OnLayerSetChange;
begin
  FPrepareResultTask.StopExecute;
  FPrepareResultTask.StartExecute;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.OnMapVersionChange;
begin
  FPrepareResultTask.StopExecute;
  FPrepareResultTask.StartExecute;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.OnPosChange;
begin
  FPrepareResultTask.StopExecute;
  FPrepareResultTask.StartExecute;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.AddVersionListener(
  const ALayerSet: IMapTypeSet
);
var
  VMap: IMapType;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  Vcnt: Cardinal;
begin
  if Assigned(ALayerSet) then begin
    VEnum := ALayerSet.GetIterator;
    while VEnum.Next(1, VGUID, Vcnt) = S_OK do begin
      VMap := ALayerSet.GetMapTypeByGUID(VGUID);
      if VMap <> nil then begin
        VMap.VersionRequestConfig.ChangeNotifier.Add(FVersionListener);
      end;
    end;
  end;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.AddLayerListeners(
  const ALocalConverter: ILocalCoordConverter;
  const ALayerSet: IMapTypeSet
);
var
  VListeners: IInterfaceListSimple;
  VListener: IListener;
  VMap: IMapType;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  Vcnt: Cardinal;
  i: Integer;
  VNotifier: INotifierTilePyramidUpdate;
  VZoom: Byte;
  VConverter: ICoordConverter;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VMapLonLatRect: TDoubleRect;
  VTileRect: TRect;
begin
  if ALayerSet <> nil then begin
    if ALayerSet.GetCount > 0 then begin
      if FLayerListeners = nil then begin
        VListeners := TInterfaceListSimple.Create;
        VListeners.Capacity := ALayerSet.GetCount;
        VEnum := ALayerSet.GetIterator;
        while VEnum.Next(1, VGUID, Vcnt) = S_OK do begin
          VMap := ALayerSet.GetMapTypeByGUID(VGUID);
          Assert(Assigned(VMap));
          if VMap <> nil then begin
            VListener := TTileUpdateListenerToLonLat.Create(VMap.GeoConvert, Self.OnTileUpdate);
            VListeners.Add(VListener);
          end;
        end;
        FLayerListeners := VListeners.MakeStaticAndClear;
      end;
      VZoom := ALocalConverter.Zoom;
      VConverter := ALocalConverter.GeoConverter;
      VMapRect := ALocalConverter.GetRectInMapPixelFloat;
      VConverter.CheckPixelRectFloat(VMapRect, VZoom);
      VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
      VEnum := ALayerSet.GetIterator;
      i := 0;
      while VEnum.Next(1, VGUID, Vcnt) = S_OK do begin
        VMap := ALayerSet.GetMapTypeByGUID(VGUID);
        if VMap <> nil then begin
          VNotifier := VMap.TileStorage.TileNotifier;
          if VNotifier <> nil then begin
            VConverter := VMap.GeoConvert;
            VMapLonLatRect := VLonLatRect;
            VConverter.CheckLonLatRect(VMapLonLatRect);
            VTileRect :=
              RectFromDoubleRect(
                VConverter.LonLatRect2TileRectFloat(VMapLonLatRect, VZoom),
                rrToTopLeft
              );
            VNotifier.AddListenerByRect(IListener(FLayerListeners[i]), VZoom, VTileRect);
          end;
          Inc(i);
        end;
      end;
    end;
  end;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.RemoveLayerListeners(
  const ALayerSet: IMapTypeSet
);
var
  VNotifier: INotifierTilePyramidUpdate;
  i: Integer;
  VMap: IMapType;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  Vcnt: Cardinal;
begin
  if Assigned(ALayerSet) then begin
    VEnum := ALayerSet.GetIterator;
    i := 0;
    while VEnum.Next(1, VGUID, Vcnt) = S_OK do begin
      VMap := ALayerSet.GetMapTypeByGUID(VGUID);
      if VMap <> nil then begin
        VNotifier := VMap.TileStorage.TileNotifier;
        if VNotifier <> nil then begin
          VNotifier.Remove(IListener(FLayerListeners.Items[i]));
        end;
        Inc(i);
      end;
    end;
  end;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.RemoveVersionListener(
  const ALayerSet: IMapTypeSet);
var
  VMap: IMapType;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  Vcnt: Cardinal;
begin
  if Assigned(ALayerSet) then begin
    VEnum := ALayerSet.GetIterator;
    while VEnum.Next(1, VGUID, Vcnt) = S_OK do begin
      VMap := ALayerSet.GetMapTypeByGUID(VGUID);
      if VMap <> nil then begin
        VMap.VersionRequestConfig.ChangeNotifier.Remove(FVersionListener);
      end;
    end;
  end;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.OnPrepareSubset(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VNeedRedraw: Boolean;
  VLocalConverter: ILocalCoordConverter;
  VLayerSet: IMapTypeSet;
  VCounterContext: TInternalPerformanceCounterContext;
  VResult: IVectorItemSubset;
  VNeedNotify: Boolean;
begin
  FDelicateUpdateFlag.CheckFlagAndReset;
  VNeedRedraw := True;
  while VNeedRedraw do begin
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;
    VLocalConverter := FPosition.GetStatic;
    VLayerSet := FLayersSet.GetStatic;
    if not VLayerSet.IsEqual(FPrevLayerSet) then begin
      RemoveLayerListeners(FPrevLayerSet);
      RemoveVersionListener(FPrevLayerSet);
      FLayerListeners := nil;
      FPrevLayerSet := VLayerSet;
      FPrevLocalConverter := VLocalConverter;
      AddVersionListener(VLayerSet);
      AddLayerListeners(VLocalConverter, VLayerSet);
    end else if not VLocalConverter.GetIsSameConverter(FPrevLocalConverter) then begin
      RemoveLayerListeners(FPrevLayerSet);
      AddLayerListeners(VLocalConverter, VLayerSet);
      FPrevLocalConverter := VLocalConverter;
    end;

    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;
    VCounterContext := FSubsetPrepareCounter.StartOperation;
    try
      VResult := PrepareSubset(AOperationID, ACancelNotifier, VLocalConverter, VLayerSet);
    finally
      FSubsetPrepareCounter.FinishOperation(VCounterContext);
    end;
    FResultCS.BeginWrite;
    try
      if FResult = nil then begin
        VNeedNotify := (VResult <> nil) and (not VResult.IsEmpty);
      end else begin
        VNeedNotify := not FResult.IsEqual(VResult);
      end;
      FResult := VResult;
    finally
      FResultCS.EndWrite;
    end;
    if VNeedNotify then begin
      DoChangeNotify;
    end;
    VNeedRedraw := FDelicateUpdateFlag.CheckFlagAndReset;
  end;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.OnTileUpdate(
  const AMsg: IInterface
);
begin
  FDelicateUpdateFlag.SetFlag;
  FPrepareResultTask.StartExecute;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.AddElementsFromMap(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AElments: IVectorItemSubsetBuilder;
  const AAlayer: IMapType;
  const AVersion: IMapVersionRequest;
  const ALocalConverter: ILocalCoordConverter
);
var
  i: integer;
  VItems: IVectorItemSubset;
  VTileIterator: ITileIterator;
  VZoom: Byte;
  VSourceGeoConvert: ICoordConverter;
  VGeoConvert: ICoordConverter;
  VBitmapOnMapPixelRect: TDoubleRect;
  VSourceLonLatRect: TDoubleRect;
  VTileSourceRect: TRect;
  VTile: TPoint;
  VErrorString: string;
  VError: ITileErrorInfo;
begin
  VZoom := ALocalConverter.GetZoom;
  VSourceGeoConvert := AAlayer.GeoConvert;
  VGeoConvert := ALocalConverter.GetGeoConverter;

  VBitmapOnMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
  VGeoConvert.CheckPixelRectFloat(VBitmapOnMapPixelRect, VZoom);

  VSourceLonLatRect := VGeoConvert.PixelRectFloat2LonLatRect(VBitmapOnMapPixelRect, VZoom);
  VTileSourceRect :=
    RectFromDoubleRect(
      VSourceGeoConvert.LonLatRect2TileRectFloat(VSourceLonLatRect, VZoom),
      rrToTopLeft
    );
  VTileIterator := TTileIteratorByRect.Create(VTileSourceRect);

  while VTileIterator.Next(VTile) do begin
    VErrorString := '';
    try
      VItems := AAlayer.LoadTileVector(VTile, VZoom, AVersion, False, AAlayer.CacheVector);
      if VItems <> nil then begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Break;
        end else begin
          for i := 0 to VItems.Count - 1 do begin
            AddWikiElement(AElments, VItems.GetItem(i), ALocalConverter);
          end;
        end;
      end;
    except
      on E: Exception do begin
        VErrorString := E.Message;
      end;
      else
        VErrorString := SAS_ERR_TileDownloadUnexpectedError;
    end;
    if VErrorString <> '' then begin
      VError :=
        TTileErrorInfo.Create(
          AAlayer.Zmp.GUID,
          VZoom,
          VTile,
          VErrorString
        );
      FErrorLogger.LogError(VError);
    end;
    VItems := nil;
  end;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.AddWikiElement(
  const AElments: IVectorItemSubsetBuilder;
  const AData: IVectorDataItemSimple;
  const ALocalConverter: ILocalCoordConverter
);
var
  VConverter: ICoordConverter;
  VSize: TPoint;
  VRect: ILonLatRect;
  VLLRect: TDoubleRect;
  VBounds: TDoubleRect;
begin
  if AData <> nil then begin
    VSize := ALocalConverter.GetLocalRectSize;
    VConverter := ALocalConverter.GetGeoConverter;
    VRect := AData.Geometry.Bounds;
    if VRect <> nil then begin
      VLLRect := VRect.Rect;
      VConverter.CheckLonLatRect(VLLRect);
      VBounds := ALocalConverter.LonLatRect2LocalRectFloat(VLLRect);
      if ((VBounds.Top < VSize.Y) and (VBounds.Bottom > 0) and (VBounds.Left < VSize.X) and (VBounds.Right > 0)) then begin
        if Supports(AData.Geometry, IGeometryLonLatPoint) or (((VBounds.Right - VBounds.Left) > 1) and ((VBounds.Bottom - VBounds.Top) > 1)) then begin
          AElments.Add(AData);
        end;
      end;
    end;
  end;
end;

function TVectorItemSubsetChangeableForVectorLayers.PrepareSubset(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter;
  const ALayerSet: IMapTypeSet
): IVectorItemSubset;
var
  VEnum: IEnumGUID;
  VGUID: TGUID;
  Vcnt: Cardinal;
  VMapType: IMapType;
  VElements: IVectorItemSubsetBuilder;
begin
  VElements := FVectorItemSubsetBuilderFactory.Build;
  if ALayerSet <> nil then begin
    VEnum := ALayerSet.GetIterator;
    while VEnum.Next(1, VGUID, Vcnt) = S_OK do begin
      VMapType := ALayerSet.GetMapTypeByGUID(VGUID);
      if VMapType.IsKmlTiles then begin
        AddElementsFromMap(
          AOperationID,
          ACancelNotifier,
          VElements,
          VMapType,
          VMapType.VersionRequestConfig.GetStatic,
          ALocalConverter
        );
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Break;
        end;
      end;
    end;
  end;
  VElements.RemoveDuplicates;
  Result := VElements.MakeStaticAndClear;
end;

end.
