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

unit u_BitmapLayerProviderChangeableForMarksLayer;

interface

uses
  i_MarksDrawConfig,
  i_Bitmap32BufferFactory,
  i_VectorItemSubsetChangeable,
  i_GeometryProjectedProvider,
  i_ListenerNotifierLinksList,
  i_MarkerProviderForVectorItem,
  i_BitmapLayerProvider,
  u_BitmapLayerProviderChangeableBase;

type
  TBitmapLayerProviderChangeableForMarksLayer = class(TBitmapLayerProviderChangeableBase)
  private
    FConfig: IMarksDrawConfig;
    FBitmapFactory: IBitmap32BufferFactory;
    FProjectedProvider: IGeometryProjectedProvider;
    FMarkerProvider: IMarkerProviderForVectorItem;
    FVectorItems: IVectorItemSubsetChangeable;

    procedure OnConfigChange;
    procedure OnVectorItemsChange;
  protected
    function CreateStatic: IInterface; override;
  public
    constructor Create(
      const AConfig: IMarksDrawConfig;
      const ABitmapFactory: IBitmap32BufferFactory;
      const AProjectedProvider: IGeometryProjectedProvider;
      const AMarkerProvider: IMarkerProviderForVectorItem;
      const AVectorItems: IVectorItemSubsetChangeable
    );
  end;

implementation

uses
  i_VectorItemSubset,
  u_ListenerByEvent,
  u_BitmapLayerProviderByMarksSubset;

{ TBitmapLayerProviderChangeableForMarksLayer }

constructor TBitmapLayerProviderChangeableForMarksLayer.Create(
  const AConfig: IMarksDrawConfig;
  const ABitmapFactory: IBitmap32BufferFactory;
  const AProjectedProvider: IGeometryProjectedProvider;
  const AMarkerProvider: IMarkerProviderForVectorItem;
  const AVectorItems: IVectorItemSubsetChangeable
);
begin
  inherited Create;
  FConfig := AConfig;
  FBitmapFactory := ABitmapFactory;
  FProjectedProvider := AProjectedProvider;
  FMarkerProvider := AMarkerProvider;
  FVectorItems := AVectorItems;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnVectorItemsChange),
    FVectorItems.ChangeNotifier
  );
end;

function TBitmapLayerProviderChangeableForMarksLayer.CreateStatic: IInterface;
var
  VVectorItems: IVectorItemSubset;
  VResult: IBitmapLayerProvider;
begin
  VResult := nil;
  VVectorItems := FVectorItems.GetStatic;
  if Assigned(VVectorItems) and not VVectorItems.IsEmpty then begin
    VResult :=
      TBitmapLayerProviderByMarksSubset.Create(
        FConfig.DrawOrderConfig.GetStatic,
        FConfig.CaptionDrawConfig.GetStatic,
        FBitmapFactory,
        FProjectedProvider,
        FMarkerProvider,
        VVectorItems
      );
  end;
  Result := VResult;
end;

procedure TBitmapLayerProviderChangeableForMarksLayer.OnConfigChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapLayerProviderChangeableForMarksLayer.OnVectorItemsChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.

