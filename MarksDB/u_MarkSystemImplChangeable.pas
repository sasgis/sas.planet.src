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

unit u_MarkSystemImplChangeable;

interface

uses
  i_PathConfig,
  i_Listener,
  i_MarkPicture,
  i_HashFunction,
  i_GeometryLonLatFactory,
  i_VectorItemSubsetBuilder,
  i_AppearanceOfMarkFactory,
  i_MarkFactory,
  i_NotifierOperation,
  i_BackgroundTask,
  i_InternalPerformanceCounter,
  i_HtmlToHintTextConverter,
  i_ReadWriteState,
  i_MarkSystemImpl,
  i_MarkSystemImplChangeable,
  u_ConfigDataElementBase,
  u_ReadWriteStateInternalByOther;

type
  TMarkSystemImplChangeable = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IMarkSystemImplChangeable)
  private
    FBasePath: IPathConfig;
    FMarkPictureList: IMarkPictureList;
    FHashFunction: IHashFunction;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FMarkFactory: IMarkFactory;
    FHintConverter: IHtmlToHintTextConverter;
    FLoadDbCounter: IInternalPerformanceCounter;
    FSaveDbCounter: IInternalPerformanceCounter;
    FAppStartedNotifier: INotifierOneOperation;
    FAppClosingNotifier: INotifierOneOperation;
    FBackgroundTask: IBackgroundTask;

    FState: IReadWriteStateChangeble;
    FStateInternal: IReadWriteStateInternalByOther;

    FPathChangeListener: IListener;
    FAppStartedListener: IListener;
  private
    procedure OnPathChanged;
    procedure OnAppStarted;
    procedure OnInitialization(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
  protected
    function CreateStatic: IInterface; override;
  private
    function GetState: IReadWriteStateChangeble;
    function GetStatic: IMarkSystemImpl;
  public
    constructor Create(
      const ABasePath: IPathConfig;
      const AMarkPictureList: IMarkPictureList;
      const AHashFunction: IHashFunction;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AMarkFactory: IMarkFactory;
      const ALoadDbCounter: IInternalPerformanceCounter;
      const ASaveDbCounter: IInternalPerformanceCounter;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const AHintConverter: IHtmlToHintTextConverter
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  u_BackgroundTask,
  u_ThreadConfig,
  u_ListenerByEvent,
  u_MarkSystemSml;

{ TMarksSystemImplChangeable }

constructor TMarkSystemImplChangeable.Create(
  const ABasePath: IPathConfig;
  const AMarkPictureList: IMarkPictureList;
  const AHashFunction: IHashFunction;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AMarkFactory: IMarkFactory;
  const ALoadDbCounter: IInternalPerformanceCounter;
  const ASaveDbCounter: IInternalPerformanceCounter;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const AHintConverter: IHtmlToHintTextConverter
);
begin
  Assert(ABasePath <> nil);
  inherited Create;
  FBasePath := ABasePath;
  FMarkPictureList := AMarkPictureList;
  FHashFunction := AHashFunction;
  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FMarkFactory := AMarkFactory;
  FLoadDbCounter := ALoadDbCounter;
  FSaveDbCounter := ASaveDbCounter;
  FHintConverter := AHintConverter;
  FAppStartedNotifier := AAppStartedNotifier;
  FAppClosingNotifier := AAppClosingNotifier;
  FStateInternal := TReadWriteStateInternalByOther.Create;
  FState := FStateInternal;

  FBackgroundTask := nil;

  FPathChangeListener := TNotifyNoMmgEventListener.Create(Self.OnPathChanged);
  FBasePath.ChangeNotifier.Add(FPathChangeListener);

  FAppStartedListener := TNotifyNoMmgEventListener.Create(Self.OnAppStarted);
  FAppStartedNotifier.Add(FAppStartedListener);
  OnAppStarted;
end;

destructor TMarkSystemImplChangeable.Destroy;
begin
  if Assigned(FBasePath) and Assigned(FPathChangeListener) then begin
    FBasePath.ChangeNotifier.Remove(FPathChangeListener);
    FBasePath := nil;
  end;
  if Assigned(FAppStartedNotifier) and Assigned(FAppStartedListener) then begin
    FAppStartedNotifier.Remove(FAppStartedListener);
    FAppStartedNotifier := nil;
  end;
  if Assigned(FBackgroundTask) then begin
    FBackgroundTask.StopExecute;
    FBackgroundTask := nil;
  end;
  inherited;
end;

function TMarkSystemImplChangeable.CreateStatic: IInterface;
var
  VStatic: IMarkSystemImpl;
begin
  VStatic := nil;
  if FAppStartedNotifier.IsExecuted then begin
    VStatic :=
      TMarkSystemSml.Create(
        FBasePath.FullPath,
        FMarkPictureList,
        FHashFunction,
        FAppearanceOfMarkFactory,
        FVectorGeometryLonLatFactory,
        FVectorItemSubsetBuilderFactory,
        FMarkFactory,
        FLoadDbCounter,
        FSaveDbCounter,
        FHintConverter
      );
  end;
  if VStatic <> nil then begin
    if VStatic.IsInitializationRequired then begin
      FBackgroundTask :=
        TBackgroundTask.Create(
          FAppClosingNotifier,
          Self.OnInitialization,
          TThreadConfig.Create(tpNormal),
          Self.ClassName
        );
      FBackgroundTask.Start;
      FBackgroundTask.StartExecute;
    end else begin
      FStateInternal.SetOther(VStatic.State);
    end;
  end else begin
    FStateInternal.SetOther(nil);
  end;
  Result := VStatic;
end;

procedure TMarkSystemImplChangeable.OnInitialization(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VStatic: IMarkSystemImpl;
begin
  try
    VStatic := Self.GetStatic;
    VStatic.Initialize(AOperationID, ACancelNotifier);
    FStateInternal.SetOther(VStatic.State);
  finally
    FBackgroundTask.StopExecute;
    FBackgroundTask.Terminate;
    FBackgroundTask := nil;
  end;
end;

function TMarkSystemImplChangeable.GetState: IReadWriteStateChangeble;
begin
  Result := FState;
end;

function TMarkSystemImplChangeable.GetStatic: IMarkSystemImpl;
begin
  Result := IMarkSystemImpl(GetStaticInternal);
end;

procedure TMarkSystemImplChangeable.OnAppStarted;
begin
  if (FAppStartedNotifier <> nil) and FAppStartedNotifier.IsExecuted then begin
    LockWrite;
    try
      SetChanged;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TMarkSystemImplChangeable.OnPathChanged;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
