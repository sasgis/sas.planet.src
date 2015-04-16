{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_VectorTileRendererChangeableForMarksLayer;

interface

uses
  i_MarksDrawConfig,
  i_Bitmap32BufferFactory,
  i_GeometryProjectedProvider,
  i_ListenerNotifierLinksList,
  i_MarkerProviderForVectorItem,
  i_VectorTileRenderer,
  i_VectorTileRendererChangeable,
  u_ChangeableBase;

type
  TVectorTileRendererChangeableForMarksLayer = class(TChangeableWithSimpleLockBase, IVectorTileRendererChangeable)
  private
    FDrawOrderConfig: IMarksDrawOrderConfig;
    FCaptionDrawConfig: ICaptionDrawConfig;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FProjectedProvider: IGeometryProjectedProvider;
    FMarkerProvider: IMarkerProviderForVectorItem;

    FLinksList: IListenerNotifierLinksList;
    FResult: IVectorTileRenderer;
    procedure OnConfigChange;
  private
    function GetStatic: IVectorTileRenderer;
  public
    constructor Create(
      const ADrawOrderConfig: IMarksDrawOrderConfig;
      const ACaptionDrawConfig: ICaptionDrawConfig;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AProjectedProvider: IGeometryProjectedProvider;
      const AMarkerProvider: IMarkerProviderForVectorItem
    );
  end;

implementation

uses
  u_ListenerByEvent,
  u_ListenerNotifierLinksList,
  u_VectorTileRendererForMarks;

{ TBitmapLayerProviderChangeableForMarksLayer }

constructor TVectorTileRendererChangeableForMarksLayer.Create(
  const ADrawOrderConfig: IMarksDrawOrderConfig;
  const ACaptionDrawConfig: ICaptionDrawConfig;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AProjectedProvider: IGeometryProjectedProvider;
  const AMarkerProvider: IMarkerProviderForVectorItem
);
begin
  Assert(Assigned(ADrawOrderConfig));
  Assert(Assigned(ACaptionDrawConfig));
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AProjectedProvider));
  Assert(Assigned(AMarkerProvider));
  inherited Create;
  FDrawOrderConfig := ADrawOrderConfig;
  FCaptionDrawConfig := ACaptionDrawConfig;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FProjectedProvider := AProjectedProvider;
  FMarkerProvider := AMarkerProvider;

  FLinksList := TListenerNotifierLinksList.Create;

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FDrawOrderConfig.ChangeNotifier
  );
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FCaptionDrawConfig.ChangeNotifier
  );
  FLinksList.ActivateLinks;
  OnConfigChange;
end;

function TVectorTileRendererChangeableForMarksLayer.GetStatic: IVectorTileRenderer;
begin
  CS.BeginRead;
  try
    Result := FResult;
  finally
    CS.EndRead;
  end;
end;

procedure TVectorTileRendererChangeableForMarksLayer.OnConfigChange;
var
  VResult: IVectorTileRenderer;
begin
  VResult :=
    TVectorTileRendererForMarks.Create(
      FDrawOrderConfig.GetStatic,
      FCaptionDrawConfig.GetStatic,
      FBitmap32StaticFactory,
      FProjectedProvider,
      FMarkerProvider
    );

  CS.BeginWrite;
  try
    FResult := VResult;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

end.
