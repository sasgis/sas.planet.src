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

unit u_ThreadRegionProcessAbstract;

interface

uses
  i_GeometryLonLat,
  i_NotifierOperation,
  i_RegionProcessTask,
  i_RegionProcessProgressInfo,
  u_BaseInterfacedObject;

type
  TRegionProcessTaskAbstract = class(TBaseInterfacedObject, IRegionProcessTask)
  private
    FCancelNotifier: INotifierOperation;
    FOperationID: Integer;
    FProgressInfo: IRegionProcessProgressInfoInternal;
    FPolygLL: IGeometryLonLatPolygon;
  protected
    procedure ProgressFormUpdateOnProgress(AProcessed, AToProcess: Int64);
    procedure ProcessRegion; virtual; abstract;

    property CancelNotifier: INotifierOperation read FCancelNotifier;
    property OperationID: Integer read FOperationID;
    property ProgressInfo: IRegionProcessProgressInfoInternal read FProgressInfo;
    property PolygLL: IGeometryLonLatPolygon read FPolygLL;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APolygon: IGeometryLonLatPolygon
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_ResStrings;

{ TRegionProcessTaskAbstract }

constructor TRegionProcessTaskAbstract.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolygon: IGeometryLonLatPolygon
);
begin
  inherited Create;
  FCancelNotifier := AProgressInfo.CancelNotifier;
  FOperationID := AProgressInfo.OperationID;
  FProgressInfo := AProgressInfo;
  FPolygLL := APolygon;
end;

destructor TRegionProcessTaskAbstract.Destroy;
begin
  if Assigned(FProgressInfo) then begin
    FProgressInfo.Finish;
    FProgressInfo := nil;
  end;
  inherited;
end;

procedure TRegionProcessTaskAbstract.ProgressFormUpdateOnProgress(
  AProcessed, AToProcess: Int64
);
begin
  ProgressInfo.SetProcessedRatio(AProcessed / AToProcess);
  ProgressInfo.SetSecondLine(SAS_STR_Processed + ' ' + inttostr(AProcessed));
end;

end.
