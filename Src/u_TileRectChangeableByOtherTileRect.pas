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

unit u_TileRectChangeableByOtherTileRect;

interface

uses
  SysUtils,
  i_TileRect,
  i_TileRectChangeable,
  i_ProjectionSet,
  i_CoordConverterFactory,
  i_Listener,
  u_ChangeableBase;

type
  TTileRectChangeableByOtherTileRect = class(TChangeableBase, ITileRectChangeable)
  private
    FSource: ITileRectChangeable;
    FResultProjectionSet: IProjectionSet;
    FMainLock: IReadWriteSync;
    FResultLock: IReadWriteSync;
    FListener: IListener;

    FPrevSource: ITileRect;
    FResult: ITileRect;
    procedure OnConverterChanged;
  private
    function GetStatic: ITileRect;
  public
    constructor Create(
      const ASource: ITileRectChangeable;
      const AResultProjectionSet: IProjectionSet;
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
  u_GeoFunc,
  u_ListenerByEvent,
  u_TileRect;

{ TTileRectChangeableByOtherTileRect }

constructor TTileRectChangeableByOtherTileRect.Create(
  const ASource: ITileRectChangeable;
  const AResultProjectionSet: IProjectionSet;
  const AMainLock, AResultLock: IReadWriteSync
);
begin
  Assert(Assigned(ASource));
  Assert(Assigned(AResultProjectionSet));
  Assert(Assigned(AMainLock));
  Assert(Assigned(AResultLock));
  inherited Create(AMainLock);
  FSource := ASource;
  FResultProjectionSet := AResultProjectionSet;
  FMainLock := AMainLock;
  FResultLock := AResultLock;
  FPrevSource := nil;
  FListener := TNotifyNoMmgEventListener.Create(Self.OnConverterChanged);
end;

destructor TTileRectChangeableByOtherTileRect.Destroy;
begin
  if Assigned(FSource) and Assigned(FListener) then begin
    FSource.ChangeNotifier.Remove(FListener);
    FListener := nil;
    FSource := nil;
  end;
  inherited;
end;

procedure TTileRectChangeableByOtherTileRect.AfterConstruction;
begin
  inherited;
  FSource.ChangeNotifier.Add(FListener);
  OnConverterChanged;
end;

function TTileRectChangeableByOtherTileRect.GetStatic: ITileRect;
begin
  FResultLock.BeginRead;
  try
    Result := FResult;
  finally
    FResultLock.EndRead;
  end;
end;

procedure TTileRectChangeableByOtherTileRect.OnConverterChanged;
var
  VSource: ITileRect;
  VResult: ITileRect;
  VChanged: Boolean;
  VSourceProjection: IProjectionInfo;
  VLonLatRect: TDoubleRect;
  VTileRectFloat: TDoubleRect;
  VTileRect: TRect;
  VProjection: IProjectionInfo;
begin
  VChanged := False;
  FMainLock.BeginWrite;
  try
    VSource := FSource.GetStatic;
    if Assigned(VSource) then begin
      if not VSource.IsEqual(FPrevSource) then begin
        VSourceProjection := VSource.ProjectionInfo;
        if FResultProjectionSet.IsProjectionFromThisSet(VSourceProjection) then begin
          VResult := VSource;
        end else begin
          VProjection := FResultProjectionSet.GetSuitableProjection(VSourceProjection);
          Assert(VSourceProjection.CheckTileRect(VSource.Rect));
          VLonLatRect := VSourceProjection.TileRect2LonLatRect(VSource.Rect);
          VProjection.ProjectionType.ValidateLonLatRect(VLonLatRect);
          VTileRectFloat := VProjection.LonLatRect2TileRectFloat(VLonLatRect);
          VTileRect := RectFromDoubleRect(VTileRectFloat, rrOutside);
          Assert(VProjection.CheckTileRect(VTileRect));
          VResult := TTileRect.Create(VProjection, VTileRect);
          Assert(Assigned(VResult));
        end;
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
      if Assigned(FSource) then begin
        FResultLock.BeginWrite;
        try
          FResult := nil;
          VChanged := True;
        finally
          FResultLock.EndWrite;
        end;
      end;
    end;
    FPrevSource := VSource;
  finally
    FMainLock.EndWrite;
  end;
  if VChanged then begin
    DoChangeNotify;
  end;
end;

end.
