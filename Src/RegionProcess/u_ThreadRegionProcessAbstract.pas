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
  Classes,
  i_Listener,
  i_GeometryLonLat,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  u_BaseInterfacedObject;

type
  IRegionProcessTask = interface
    ['{F1502271-EBD9-408E-BF77-5E0204CCE6FD}']
    procedure ProcessRegion;
  end;

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

  TRegionProcessWorker = class(TThread)
  private
    FTask: IRegionProcessTask;
    FOperationID: Integer;
    FCancelListener: IListener;

    FMessageForShow: string;
    FCancelNotifier: INotifierOperation;
    FDebugThreadName: string;
    procedure OnCancel;
    procedure SynShowMessage;
    {$HINTS OFF}
    // Disable hint: "Private symbol 'ShowMessageSync' declared but never used"
    // in case we catch exceptions by EurekaLog (see below)
    procedure ShowMessageSync(const AMessage: string);
    {$HINTS ON}
  protected
    procedure Execute; override;
  public
    procedure Start;
  public
    constructor Create(
      const ATask: IRegionProcessTask;
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const ADebugThreadName: string = ''
    );
    destructor Destroy; override;
  end;



implementation

uses
  {$IFDEF EUREKALOG}
  ExceptionLog,
  {$ENDIF}
  SysUtils,
  Dialogs,
  u_ResStrings,
  u_ReadableThreadNames,
  u_ListenerByEvent;

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

{ TRegionProcessWorker }

constructor TRegionProcessWorker.Create(
  const ATask: IRegionProcessTask;
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const ADebugThreadName: string
);
begin
  Assert(Assigned(ATask));
  Assert(Assigned(AProgressInfo));
  inherited Create(True);
  FTask := ATask;
  FDebugThreadName := ADebugThreadName;
  Priority := tpLowest;
  FreeOnTerminate := true;
  FCancelNotifier := AProgressInfo.CancelNotifier;
  FOperationID := AProgressInfo.OperationID;
  if not FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    FCancelListener := TNotifyNoMmgEventListener.Create(Self.OnCancel);
    FCancelNotifier.AddListener(FCancelListener);
  end;
  if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    Terminate;
  end;
end;

destructor TRegionProcessWorker.Destroy;
begin
  if Assigned(FCancelListener) and Assigned(FCancelNotifier) then begin
    FCancelNotifier.RemoveListener(FCancelListener);
    FCancelListener := nil;
    FCancelNotifier := nil;
  end;
  inherited;
end;

procedure TRegionProcessWorker.Execute;
begin
  SetCurrentThreadName(FDebugThreadName);
  try
    FTask.ProcessRegion;
  except
  {$IFDEF EUREKALOG}
    ShowLastExceptionData;
  {$ELSE}
    on E: Exception do begin
      ShowMessageSync(E.ClassName + ': ' + E.Message);
    end;
  {$ENDIF}
  end;
end;

procedure TRegionProcessWorker.OnCancel;
begin
  Terminate;
end;

procedure TRegionProcessWorker.ShowMessageSync(const AMessage: string);
begin
  FMessageForShow := AMessage;
  Synchronize(SynShowMessage);
end;

procedure TRegionProcessWorker.Start;
begin
  Resume;
end;

procedure TRegionProcessWorker.SynShowMessage;
begin
  ShowMessage(FMessageForShow);
end;

end.
