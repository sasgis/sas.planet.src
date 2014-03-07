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

unit u_RegionProcessProgressInfoInternalFactory;

interface

uses
  i_GeometryLonLat,
  i_RegionProcessProgressInfo,
  i_MapViewGoto,
  i_RegionProcess,
  i_NotifierOperation,
  i_NotifierTime,
  i_RegionProcessProgressInfoInternalFactory,
  u_BaseInterfacedObject;

type
  TRegionProcessProgressInfoInternalFactory = class(TBaseInterfacedObject, IRegionProcessProgressInfoInternalFactory)
  private
    FAppClosingNotifier: INotifierOneOperation;
    FTimerNoifier: INotifierTime;
    FMapGoto: IMapViewGoto;
    FRegionProcess: IRegionProcess;
  private
    function Build(
      const APolygon: IGeometryLonLatMultiPolygon
    ): IRegionProcessProgressInfoInternal;
  public
    constructor Create(
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const ARegionProcess: IRegionProcess;
      const AMapGoto: IMapViewGoto
    );
  end;

implementation

uses
  Forms,
  u_RegionProcessProgressInfo,
  u_NotifierOperation,
  u_Notifier,
  frm_ProgressSimple;

{ TRegionProcessProgressInfoInternalFactory }

constructor TRegionProcessProgressInfoInternalFactory.Create(
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const ARegionProcess: IRegionProcess;
  const AMapGoto: IMapViewGoto
);
begin
  inherited Create;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
  FRegionProcess := ARegionProcess;
  FMapGoto := AMapGoto;
end;

function TRegionProcessProgressInfoInternalFactory.Build(
  const APolygon: IGeometryLonLatMultiPolygon
): IRegionProcessProgressInfoInternal;
var
  VCancelNotifierInternal: INotifierOperationInternal;
  VProgressInfo: TRegionProcessProgressInfo;
  VOperationID: Integer;
begin
  VCancelNotifierInternal := TNotifierOperation.Create(TNotifierBase.Create);
  VOperationID := VCancelNotifierInternal.CurrentOperation;
  VProgressInfo := TRegionProcessProgressInfo.Create(VCancelNotifierInternal, VOperationID);
  Result := VProgressInfo;

  TfrmProgressSimple.Create(
    Application,
    FAppClosingNotifier,
    FTimerNoifier,
    VCancelNotifierInternal,
    VProgressInfo,
    FRegionProcess,
    FMapGoto,
    APolygon
  );
end;

end.
