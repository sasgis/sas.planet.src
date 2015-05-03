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

unit u_MergePolygonsProcessor;

interface

uses
  Windows, // sleep
  t_MergePolygonsProcessor,
  i_BackgroundTask,
  i_VectorDataFactory,
  i_GeometryLonLatFactory,
  i_NotifierOperation;

type
  TMergePolygonsProcessor = class
  private
    FItems: TMergePolygonsItemArray;
    FOperation: TMergeOperation;
    FOnMergeFinished: TOnMergeFinished;
    FBackgroundTask: IBackgroundTask;
    FAppClosingNotifier: INotifierOneOperation;
    FVectorDataFactory: IVectorDataFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  private
    procedure OnExecute(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
  public
    procedure MergeAsync(
      const AItems: TMergePolygonsItemArray;
      const AOperation: TMergeOperation;
      const AOnMergeFinished: TOnMergeFinished
    );
    procedure AbortOperation;
  public
    constructor Create(
      const AAppClosingNotifier: INotifierOneOperation;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  i_ThreadConfig,
  i_VectorDataItemSimple,
  u_ThreadConfig,
  u_BackgroundTask;

{ TMergePolygonsProcessor }

constructor TMergePolygonsProcessor.Create(
  const AAppClosingNotifier: INotifierOneOperation;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
);
begin
  inherited Create;

  FAppClosingNotifier := AAppClosingNotifier;
  FVectorDataFactory := AVectorDataFactory;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;

  FBackgroundTask := nil;
end;

destructor TMergePolygonsProcessor.Destroy;
begin
  if Assigned(FBackgroundTask) then begin
    FBackgroundTask.StopExecute;
    FBackgroundTask.Terminate;
    FBackgroundTask := nil;
  end;
  inherited Destroy;
end;

procedure TMergePolygonsProcessor.MergeAsync(
  const AItems: TMergePolygonsItemArray;
  const AOperation: TMergeOperation;
  const AOnMergeFinished: TOnMergeFinished
);
var
  VThreadConfig: IThreadConfig;
begin
  Assert(Length(AItems) > 0);
  Assert(Assigned(AOnMergeFinished));
  
  FItems := AItems;
  FOperation := AOperation;
  FOnMergeFinished := AOnMergeFinished;

  if not Assigned(FBackgroundTask) then begin
    VThreadConfig := TThreadConfig.Create(tpNormal);

    FBackgroundTask :=
      TBackgroundTask.Create(
        FAppClosingNotifier,
        Self.OnExecute,
        VThreadConfig,
        Self.ClassName
      );

    FBackgroundTask.Start;
  end else begin
    FBackgroundTask.StopExecute;
  end;

  FBackgroundTask.StartExecute;
end;

procedure TMergePolygonsProcessor.AbortOperation;
begin
  FBackgroundTask.StopExecute;
end;

procedure TMergePolygonsProcessor.OnExecute(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VVectorItem: IVectorDataItem;
begin
  VVectorItem := nil;
  try

    //ToDo: Merge polygons

    VVectorItem :=
      FVectorDataFactory.BuildItem(
        FItems[0].VectorInfo,
        nil,
        FItems[0].SinglePolygon
      );

    //Sleep(10000);
    
  finally
    FOnMergeFinished(VVectorItem);
  end;
end;

end.
