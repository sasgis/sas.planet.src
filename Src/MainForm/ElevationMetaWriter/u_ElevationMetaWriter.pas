{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_ElevationMetaWriter;

interface

uses
  Classes,
  i_NotifierOperation,
  i_TerrainInfo,
  i_TerrainConfig,
  i_TerrainProviderList,
  i_GeometryLonLat,
  i_BackgroundTask,
  i_LanguageManager,
  i_ElevationMetaWriter,
  i_ElevationMetaWriterProgress,
  i_GeometryLonLatFactory,
  frm_ElevationMetaWriterProgress,
  u_BaseInterfacedObject;

type
  TElevationMetaWriter = class(TBaseInterfacedObject, IElevationMetaWriter)
  private
    FLanguageManager: ILanguageManager;
    FAppClosingNotifier: INotifierOneOperation;
    FTerrainInfo: ITerrainInfo;
    FGeometryLonLatFactory: IGeometryLonLatFactory;

    FBackgroundTask: IBackgroundTask;

    FLine: IGeometryLonLatLine;
    FResultLine: IGeometryLonLatLine;
    FOnResult: TElevationMetaWriterResult;

    FProgress: IElevationMetaWriterProgress;
    FfrmElevationMetaWriterProgress: TfrmElevationMetaWriterProgress;

    procedure OnExecute(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
    procedure OnFinish;
  private
    { IElevationMetaWriter }
    procedure ProcessLineAsync(
      const ALine: IGeometryLonLatLine;
      const AOnResult: TElevationMetaWriterResult
    );
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATerrainConfig: ITerrainConfig;
      const ATerrainProviderList: ITerrainProviderList;
      const AGeometryLonLatFactory: IGeometryLonLatFactory
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  t_GeoTypes,
  i_DoublePoints,
  i_DoublePointsAggregator,
  i_ThreadConfig,
  i_EnumDoublePoint,
  u_Dialogs,
  u_BackgroundTask,
  u_DoublePointsAggregator,
  u_ElevationMetaWriterProgress,
  u_GeometryFunc,
  u_TerrainInfo,
  u_ThreadConfig;

resourcestring
  rsWaitUntilTheFinishOperation = 'Please wait until the finish of the previous operation!';

{ TElevationMetaWriter }

constructor TElevationMetaWriter.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATerrainConfig: ITerrainConfig;
  const ATerrainProviderList: ITerrainProviderList;
   const AGeometryLonLatFactory: IGeometryLonLatFactory
);
begin
  inherited Create;

  FLanguageManager := ALanguageManager;
  FAppClosingNotifier := AAppClosingNotifier;
  FTerrainInfo := TTerrainInfo.Create(ATerrainConfig, ATerrainProviderList);
  FGeometryLonLatFactory := AGeometryLonLatFactory;

  FProgress := TElevationMetaWriterProgress.Create;

  FfrmElevationMetaWriterProgress := nil;
end;

destructor TElevationMetaWriter.Destroy;
begin
  FreeAndNil(FfrmElevationMetaWriterProgress);
  inherited Destroy;
end;

procedure TElevationMetaWriter.ProcessLineAsync(
  const ALine: IGeometryLonLatLine;
  const AOnResult: TElevationMetaWriterResult
);
begin
  // executed in the main thread

  if FProgress.Status <> emwIdle then begin
    ShowInfoMessage(rsWaitUntilTheFinishOperation);
    Exit;
  end;

  if FBackgroundTask <> nil then begin
    FBackgroundTask.StopExecute;
  end else begin
    FBackgroundTask :=
      TBackgroundTask.Create(
        FAppClosingNotifier,
        Self.OnExecute,
        TThreadConfig.Create(tpNormal) as IThreadConfig,
        Self.ClassName
      );
    FBackgroundTask.Start;
  end;

  FLine := ALine;
  FOnResult := AOnResult;

  FProgress.Reset;
  FProgress.Status := emwBusy;

  if FfrmElevationMetaWriterProgress = nil then begin
    FfrmElevationMetaWriterProgress :=
      TfrmElevationMetaWriterProgress.Create(
        FAppClosingNotifier,
        FProgress
      );
  end;
  FfrmElevationMetaWriterProgress.ShowProgress;

  FBackgroundTask.StartExecute;
end;

procedure TElevationMetaWriter.OnExecute(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);

  function IsCanceled: Boolean;
  begin
    Result := ACancelNotifier.IsOperationCanceled(AOperationID);
    if Result then begin
      FProgress.Status := emwCanceled;
    end else begin
      Result := (FProgress.Status = emwCanceled);
    end;
  end;

var
  I: Integer;
  VPointsCountMax: Integer;
  VInfo: TElevationMetaWriterProgressInfo;
  VLines: TArrayOfGeometryLonLatSingleLine;
  VEnum: IEnumLonLatPoint;
  VPoint: TDoublePoint;
  VMeta: TDoublePointsMetaItem;
  VPoints: array of IDoublePoints;
  VAggregator: IDoublePointsAggregator;
  VLineBuilder: IGeometryLonLatLineBuilder;
begin
  // executed in the backgroud thread
  FResultLine := nil;
  VPointsCountMax := 0;
  VInfo := FProgress.Info;
  try
    // prepare
    VLines := GeometryLonLatLineToArray(FLine);

    for I := 0 to Length(VLines) - 1 do begin
      Inc(VInfo.TotalCount, VLines[I].Count);
      if VLines[I].Count > VPointsCountMax then begin
        VPointsCountMax := VLines[I].Count;
      end;
    end;

    FProgress.Info := VInfo;

    // process points
    VAggregator := TDoublePointsAggregator.Create(VPointsCountMax);
    SetLength(VPoints, Length(VLines));

    for I := 0 to Length(VLines) - 1 do begin
      VEnum := VLines[I].GetEnum;
      while VEnum.Next(VPoint, VMeta) do begin
        VMeta.Elevation := FTerrainInfo.GetElevationInfo(VPoint, 23);
        VMeta.IsElevationOk := True;

        VAggregator.Add(VPoint, @VMeta);

        Inc(VInfo.ReadyCount);

        if VInfo.ReadyCount mod 512 = 0 then begin
          if IsCanceled then begin
            Exit;
          end;
          FProgress.Info := VInfo;
        end;
      end;

      VPoints[I] := VAggregator.MakeStaticAndClear;
    end;

    if IsCanceled then begin
      Exit;
    end;

    FProgress.Info := VInfo;

    // build result
    VLineBuilder := FGeometryLonLatFactory.MakeLineBuilder;

    for I := 0 to Length(VPoints) - 1 do begin
      VLineBuilder.AddLine(VPoints[I]);
    end;

    FResultLine := VLineBuilder.MakeStaticAndClear;

    FProgress.Status := emwDone;
  finally
    TThread.Synchronize(nil, Self.OnFinish);
  end;
end;

procedure TElevationMetaWriter.OnFinish;
begin
  // executed in the main thread
  try
    FfrmElevationMetaWriterProgress.Hide;
    if FProgress.Status = emwDone then begin
      FOnResult(FResultLine);
    end;
  finally
    FLine := nil;
    FResultLine := nil;
    FProgress.Status := emwIdle;
  end;
end;

end.
