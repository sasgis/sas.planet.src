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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_BitmapLayerProviderChangeableForVectorMaps;

interface

uses
  i_VectorItemDrawConfig,
  i_Bitmap32BufferFactory,
  i_VectorItemSubsetChangeable,
  i_GeometryProjectedProvider,
  i_ListenerNotifierLinksList,
  i_MarkerDrawable,
  i_BitmapLayerProvider,
  u_BitmapLayerProviderChangeableBase;

type
  TBitmapLayerProviderChangeableForVectorMaps = class(TBitmapLayerProviderChangeableBase)
  private
    FConfig: IVectorItemDrawConfig;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FPointMarker: IMarkerDrawableChangeable;
    FProjectedProvider: IGeometryProjectedProvider;
    FVectorItems: IVectorItemSubsetChangeable;

    procedure OnConfigChange;
    procedure OnItemsUpdated;
  protected
    function CreateStatic: IInterface; override;
  public
    constructor Create(
      const AConfig: IVectorItemDrawConfig;
      const APointMarker: IMarkerDrawableChangeable;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AProjectedProvider: IGeometryProjectedProvider;
      const AVectorItems: IVectorItemSubsetChangeable
    );
  end;

implementation

uses
  i_VectorItemSubset,
  u_ListenerByEvent,
  u_BitmapLayerProviderByVectorSubset;

{ TBitmapLayerProviderChangeableForVectorMaps }

constructor TBitmapLayerProviderChangeableForVectorMaps.Create(
  const AConfig: IVectorItemDrawConfig;
  const APointMarker: IMarkerDrawableChangeable;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AProjectedProvider: IGeometryProjectedProvider;
  const AVectorItems: IVectorItemSubsetChangeable
);
begin
  Assert(Assigned(AConfig));
  Assert(Assigned(APointMarker));
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AProjectedProvider));
  Assert(Assigned(AVectorItems));
  inherited Create;
  FConfig := AConfig;
  FPointMarker := APointMarker;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FProjectedProvider := AProjectedProvider;
  FVectorItems := AVectorItems;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FPointMarker.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnItemsUpdated),
    FVectorItems.ChangeNotifier
  );
end;

function TBitmapLayerProviderChangeableForVectorMaps.CreateStatic: IInterface;
var
  VConfig: IVectorItemDrawConfigStatic;
  VResult: IBitmapLayerProvider;
  VVectorItems: IVectorItemSubset;
begin
  VResult := nil;
  VConfig := FConfig.GetStatic;

  VVectorItems := FVectorItems.GetStatic;
  if Assigned(VVectorItems) and not VVectorItems.IsEmpty then begin
    VResult :=
      TBitmapLayerProviderByVectorSubset.Create(
        VConfig.MainColor,
        VConfig.ShadowColor,
        FPointMarker.GetStatic,
        FBitmap32StaticFactory,
        FProjectedProvider,
        VVectorItems
      );
  end;
  Result := VResult;
end;

procedure TBitmapLayerProviderChangeableForVectorMaps.OnConfigChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapLayerProviderChangeableForVectorMaps.OnItemsUpdated;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
