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

unit u_TileRectChangeableByLocalConverter;

interface

uses
  SysUtils,
  i_TileRect,
  i_TileRectChangeable,
  i_ProjectionSetChangeable,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_Listener,
  u_ChangeableBase;

type
  TTileRectChangeableByLocalConverterAbstract = class(TChangeableBase, ITileRectChangeable)
  private
    FLocalCoordConverter: ILocalCoordConverterChangeable;
    FMainLock: IReadWriteSync;
    FResultLock: IReadWriteSync;
    FListener: IListener;

    FPrevLocalCoordConverter: ILocalCoordConverter;
    FResult: ITileRect;
    procedure OnConverterChanged;
    function BuildTileRect(const AConverter: ILocalCoordConverter): ITileRect; virtual; abstract;
  private
    function GetStatic: ITileRect;
  public
    constructor Create(
      const ALocalCoordConverter: ILocalCoordConverterChangeable;
      const AMainLock: IReadWriteSync;
      const AResultLock: IReadWriteSync
    );
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

  TTileRectChangeableByLocalConverterSimple = class(TTileRectChangeableByLocalConverterAbstract)
  private
    function BuildTileRect(const AConverter: ILocalCoordConverter): ITileRect; override;
  public
    constructor Create(
      const ALocalCoordConverter: ILocalCoordConverterChangeable;
      const AMainLock: IReadWriteSync;
      const AResultLock: IReadWriteSync
    );
  end;

  TTileRectChangeableByLocalConverterSmart = class(TTileRectChangeableByLocalConverterAbstract)
  private
    FProjectionSet: IProjectionSetChangeable;
    function BuildTileRect(const AConverter: ILocalCoordConverter): ITileRect; override;
  public
    constructor Create(
      const AProjectionSet: IProjectionSetChangeable;
      const ALocalCoordConverter: ILocalCoordConverterChangeable;
      const AMainLock: IReadWriteSync;
      const AResultLock: IReadWriteSync
    );
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

implementation

uses
  Types,
  Math,
  t_GeoTypes,
  i_ProjectionInfo,
  i_ProjectionSet,
  u_GeoFunc,
  u_ListenerByEvent,
  u_TileRect;

{ TTileRectChangeableByLocalConverterAbstract }

constructor TTileRectChangeableByLocalConverterAbstract.Create(
  const ALocalCoordConverter: ILocalCoordConverterChangeable;
  const AMainLock: IReadWriteSync;
  const AResultLock: IReadWriteSync
);
begin
  Assert(Assigned(ALocalCoordConverter));
  Assert(Assigned(AMainLock));
  Assert(Assigned(AResultLock));
  inherited Create(AMainLock);
  FLocalCoordConverter := ALocalCoordConverter;
  FMainLock := AMainLock;
  FResultLock := AResultLock;
  FPrevLocalCoordConverter := nil;
  FListener := TNotifyNoMmgEventListener.Create(Self.OnConverterChanged);
end;

destructor TTileRectChangeableByLocalConverterAbstract.Destroy;
begin
  if Assigned(FLocalCoordConverter) and Assigned(FListener) then begin
    FLocalCoordConverter.ChangeNotifier.Remove(FListener);
    FListener := nil;
    FLocalCoordConverter := nil;
  end;
  inherited;
end;

procedure TTileRectChangeableByLocalConverterAbstract.AfterConstruction;
begin
  inherited;
  FLocalCoordConverter.ChangeNotifier.Add(FListener);
  OnConverterChanged;
end;

function TTileRectChangeableByLocalConverterAbstract.GetStatic: ITileRect;
begin
  FResultLock.BeginRead;
  try
    Result := FResult;
  finally
    FResultLock.EndRead;
  end;
end;

procedure TTileRectChangeableByLocalConverterAbstract.OnConverterChanged;
var
  VConverter: ILocalCoordConverter;
  VResult: ITileRect;
  VChanged: Boolean;
begin
  VChanged := False;
  FMainLock.BeginWrite;
  try
    VConverter := FLocalCoordConverter.GetStatic;
    if Assigned(VConverter) then begin
      if not VConverter.GetIsSameConverter(FPrevLocalCoordConverter) then begin
        VResult := BuildTileRect(VConverter);
        Assert(Assigned(VResult));
        if not VResult.IsEqual(FResult) then begin
          FResultLock.BeginWrite;
          try
            FResult := VResult;
            VChanged := True;
          finally
            FResultLock.EndWrite;
          end;
        end;
      end;
    end else begin
      if Assigned(FLocalCoordConverter) then begin
        FResultLock.BeginWrite;
        try
          FResult := nil;
          VChanged := True;
        finally
          FResultLock.EndWrite;
        end;
      end;
    end;
    FPrevLocalCoordConverter := VConverter;
  finally
    FMainLock.EndWrite;
  end;
  if VChanged then begin
    DoChangeNotify;
  end;
end;

{ TTileRectChangeableByLocalConverterSimple }

constructor TTileRectChangeableByLocalConverterSimple.Create(
  const ALocalCoordConverter: ILocalCoordConverterChangeable;
  const AMainLock, AResultLock: IReadWriteSync
);
begin
  inherited Create(ALocalCoordConverter, AMainLock, AResultLock);
end;

function TTileRectChangeableByLocalConverterSimple.BuildTileRect(
  const AConverter: ILocalCoordConverter
): ITileRect;
var
  VPixelRect: TDoubleRect;
  VTileRectFloat: TDoubleRect;
  VTileRect: TRect;
  VProjection: IProjectionInfo;
begin
  Assert(Assigned(AConverter));
  VProjection := AConverter.ProjectionInfo;
  VPixelRect := AConverter.GetRectInMapPixelFloat;
  VProjection.ValidatePixelRectFloat(VPixelRect);
  VTileRectFloat := VProjection.PixelRectFloat2TileRectFloat(VPixelRect);
  VTileRect := RectFromDoubleRect(VTileRectFloat, rrOutside);
  Assert(VProjection.CheckTileRect(VTileRect));
  Result := TTileRect.Create(VProjection, VTileRect);
end;

{ TTileRectChangeableByLocalConverterSmart }

constructor TTileRectChangeableByLocalConverterSmart.Create(
  const AProjectionSet: IProjectionSetChangeable;
  const ALocalCoordConverter: ILocalCoordConverterChangeable;
  const AMainLock, AResultLock: IReadWriteSync
);
begin
  inherited Create(ALocalCoordConverter, AMainLock, AResultLock);
  FProjectionSet := AProjectionSet;
end;

procedure TTileRectChangeableByLocalConverterSmart.AfterConstruction;
begin
  FProjectionSet.ChangeNotifier.Add(FListener);
  inherited;
end;

destructor TTileRectChangeableByLocalConverterSmart.Destroy;
begin
  if Assigned(FProjectionSet) and Assigned(FListener) then begin
    FProjectionSet.ChangeNotifier.Remove(FListener);
    FProjectionSet := nil;
  end;
  inherited;
end;

function TTileRectChangeableByLocalConverterSmart.BuildTileRect(
  const AConverter: ILocalCoordConverter
): ITileRect;
var
  VPixelRect: TDoubleRect;
  VRelativeRect: TDoubleRect;
  VTileRectFloat: TDoubleRect;
  VTileRect: TRect;
  VScale: Double;
  VProjectionSource: IProjectionInfo;
  VProjection: IProjectionInfo;
  VProjectionPrev: IProjectionInfo;
  VProjectionSet: IProjectionSet;
  VLonLatRect: TDoubleRect;
begin
  Assert(Assigned(AConverter));
  VProjectionSet := FProjectionSet.GetStatic;
  VProjectionSource := AConverter.ProjectionInfo;
  VProjection := VProjectionSet.GetSuitableProjection(VProjectionSource);
  VPixelRect := AConverter.GetRectInMapPixelFloat;
  VProjectionSource.ValidatePixelRectFloat(VPixelRect);
  if not VProjectionSource.GetIsSameProjectionInfo(VProjection) then begin
    if VProjectionSource.ProjectionType.IsSame(VProjection.ProjectionType) then begin
      VRelativeRect := VProjectionSource.PixelRectFloat2RelativeRect(VPixelRect);
      VPixelRect := VProjection.RelativeRect2PixelRectFloat(VRelativeRect);
    end else begin
      VLonLatRect := VProjectionSource.PixelRectFloat2LonLatRect(VPixelRect);
      VProjection.ProjectionType.ValidateLonLatRect(VLonLatRect);
      VPixelRect := VProjection.LonLatRect2PixelRectFloat(VLonLatRect);
    end;
  end;
  VScale := AConverter.GetScale;
  VProjectionPrev := nil;
  if VProjectionSet.CheckZoom(VProjection.Zoom - 1) then begin
    VProjectionPrev := VProjectionSet.Zooms[VProjection.Zoom - 1];
  end;
  if (VScale > 0.9) or (not Assigned(VProjectionPrev)) then begin
    VTileRectFloat := VProjection.PixelRectFloat2TileRectFloat(VPixelRect);
    VTileRect := RectFromDoubleRect(VTileRectFloat, rrOutside);
    Assert(VProjection.CheckTileRect(VTileRect));
    Result := TTileRect.Create(VProjection, VTileRect);
  end else begin
    VRelativeRect := VProjection.PixelRectFloat2RelativeRect(VPixelRect);
    VTileRectFloat := VProjectionPrev.RelativeRect2TileRectFloat(VRelativeRect);
    VTileRect := RectFromDoubleRect(VTileRectFloat, rrOutside);
    Assert(VProjectionPrev.CheckTileRect(VTileRect));
    Result := TTileRect.Create(VProjectionPrev, VTileRect);
  end;
end;

end.
