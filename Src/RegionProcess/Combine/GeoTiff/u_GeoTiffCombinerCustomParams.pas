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

unit u_GeoTiffCombinerCustomParams;

interface

uses
  Types,
  SysUtils,
  t_GeoTIFF,
  t_Bitmap32,
  i_Projection,
  i_ProjectionSet,
  i_BitmapTileProvider,
  i_MapVersionRequest,
  i_GeoTiffCombinerCustomParams,
  i_MapType,
  i_TileStorage,
  u_BaseInterfacedObject;

type
  TGeoTiffCombinerCustomParams = class(TBaseInterfacedObject, IGeoTiffCombinerCustomParams)
  private
    FBaseZoom: Byte;
    FTileStorage: ITileStorage;
    FMapVersionRequest: IMapVersionRequest;
    FBackgroundColor: TColor32;
    FProjection: array of IProjection;
    FBitmapTileProvider: array of IBitmapTileProvider;

    FZoomArray: TByteDynArray;
    FOverviewArray: TIntegerDynArray;

    procedure PrepareTileStorage(
      const AGeoTiffOptions: TGeoTiffOptions;
      const AUseOverlays: Boolean;
      const AMapType: IMapType
    );

    procedure PrepareProjection(
      const AZoomArray: TByteDynArray;
      const AProjectionSet: IProjectionSet
    );

    procedure CheckOverviewIndex(
      const AOverviewIndex: Integer
    );
  private
    { IGeoTiffCombinerCustomParams }
    function GetOverviewCount: Integer;
    function GetProjection(const AOverviewIndex: Integer): IProjection;

    function GetBitmapTileProvider(const AOverviewIndex: Integer): IBitmapTileProvider;
    procedure SetBitmapTileProvider(const AOverviewIndex: Integer; const AValue: IBitmapTileProvider);

    function GetZoomArray: TByteDynArray;
    function GetOverviewArray: TIntegerDynArray;

    function GetOverviewIndex(const AOverview: Integer): Integer;

    function GetTileStorage: ITileStorage;
    function GetMapVersionRequest: IMapVersionRequest;
    function GetBackgroundColor: TColor32;
  public
    constructor Create(
      const AGeoTiffOptions: TGeoTiffOptions;
      const AUseOverlays: Boolean;
      const AProjection: IProjection;
      const AProjectionSet: IProjectionSet;
      const AMapType: IMapType;
      const ABackgroundColor: TColor32
    );
  end;

  EGeoTiffCombinerCustomParams = class(Exception);

implementation

uses
  i_ContentTypeInfo,
  u_GeoTiffFunc,
  u_ContentTypeFunc;

{ TGeoTiffCombinerCustomParams }

constructor TGeoTiffCombinerCustomParams.Create(
  const AGeoTiffOptions: TGeoTiffOptions;
  const AUseOverlays: Boolean;
  const AProjection: IProjection;
  const AProjectionSet: IProjectionSet;
  const AMapType: IMapType;
  const ABackgroundColor: TColor32
);
var
  I: Integer;
begin
  inherited Create;

  FBackgroundColor := ABackgroundColor;

  FBaseZoom := AProjection.Zoom;

  FZoomArray := TGeoTiffFunc.OverviewArrayToZoomArray(
    FBaseZoom, AGeoTiffOptions.Overview
  );

  if Length(FZoomArray) =  Length(AGeoTiffOptions.Overview) then begin
    FOverviewArray := AGeoTiffOptions.Overview;
  end else begin
    FOverviewArray := TGeoTiffFunc.ZoomArrayToOverviewArray(FBaseZoom, FZoomArray);
  end;

  PrepareProjection(FZoomArray, AProjectionSet);

  SetLength(FBitmapTileProvider, Length(FProjection));
  for I := 0 to Length(FBitmapTileProvider) - 1 do begin
    FBitmapTileProvider[I] := nil;
  end;

  PrepareTileStorage(AGeoTiffOptions, AUseOverlays, AMapType);
end;

procedure TGeoTiffCombinerCustomParams.PrepareProjection(
  const AZoomArray: TByteDynArray;
  const AProjectionSet: IProjectionSet
);
var
  I: Integer;
begin
  SetLength(FProjection, Length(AZoomArray));
  for I := 0 to Length(AZoomArray) - 1 do begin
    FProjection[I] := AProjectionSet.Zooms[AZoomArray[I]];
  end;
end;

procedure TGeoTiffCombinerCustomParams.PrepareTileStorage(
  const AGeoTiffOptions: TGeoTiffOptions;
  const AUseOverlays: Boolean;
  const AMapType: IMapType
);
begin
  if not AUseOverlays and
     AGeoTiffOptions.CopyRawJpegTiles and
     Assigned(AMapType) and
     IsJpegContentType(AMapType.ContentType) then
  begin
    FTileStorage := AMapType.TileStorage;
    FMapVersionRequest := AMapType.VersionRequest.GetStatic;
  end;
end;

function TGeoTiffCombinerCustomParams.GetMapVersionRequest: IMapVersionRequest;
begin
  Result := FMapVersionRequest;
end;

function TGeoTiffCombinerCustomParams.GetBackgroundColor: TColor32;
begin
  Result := FBackgroundColor;
end;

function TGeoTiffCombinerCustomParams.GetOverviewArray: TIntegerDynArray;
begin
  Result := FOverviewArray;
end;

function TGeoTiffCombinerCustomParams.GetBitmapTileProvider(
  const AOverviewIndex: Integer
): IBitmapTileProvider;
begin
  CheckOverviewIndex(AOverviewIndex);
  Result := FBitmapTileProvider[AOverviewIndex];
end;

procedure TGeoTiffCombinerCustomParams.SetBitmapTileProvider(
  const AOverviewIndex: Integer;
  const AValue: IBitmapTileProvider
);
begin
  CheckOverviewIndex(AOverviewIndex);
  FBitmapTileProvider[AOverviewIndex] := AValue;
end;

function TGeoTiffCombinerCustomParams.GetOverviewCount: Integer;
begin
  Result := Length(FOverviewArray);
end;

function TGeoTiffCombinerCustomParams.GetProjection(const AOverviewIndex: Integer): IProjection;
begin
  CheckOverviewIndex(AOverviewIndex);
  Result := FProjection[AOverviewIndex];
end;

function TGeoTiffCombinerCustomParams.GetZoomArray: TByteDynArray;
begin
  Result := FZoomArray;
end;

function TGeoTiffCombinerCustomParams.GetTileStorage: ITileStorage;
begin
  Result := FTileStorage;
end;

function TGeoTiffCombinerCustomParams.GetOverviewIndex(const AOverview: Integer): Integer;
var
  I: Integer;
  VZoom: Byte;
begin
  Result := -1;
  if TGeoTiffFunc.OverviewToZoom(FBaseZoom, AOverview, VZoom) then begin
    for I := 0 to Length(FZoomArray) - 1 do begin
      if FZoomArray[I] = VZoom then begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

procedure TGeoTiffCombinerCustomParams.CheckOverviewIndex(const AOverviewIndex: Integer);
begin
  if (AOverviewIndex < 0) or (AOverviewIndex >= Length(FOverviewArray)) then begin
    raise EGeoTiffCombinerCustomParams.CreateFmt(
      'Overview index "%d" is out of range [0..%d]', [AOverviewIndex, Length(FOverviewArray)-1]
    );
  end;
end;

end.
