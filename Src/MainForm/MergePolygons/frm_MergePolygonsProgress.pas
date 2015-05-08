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

unit frm_MergePolygonsProgress;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  i_NotifierTime,
  i_Listener,
  i_ListenerTime,
  i_NotifierOperation,
  i_MergePolygonsProgress,
  u_CommonFormAndFrameParents;

type
  TfrmMergePolygonsProgress = class(TCommonFormParent)
    btnAbort: TButton;
    lblProgress: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnAbortClick(Sender: TObject);
  private
    FLastOnTimer: Cardinal;
    FAppClosingNotifier: INotifierOneOperation;
    FTimerNoifier: INotifierTime;
    FAppClosingListener: IListener;
    FTimerListener: IListenerTime;
    FCancelListener: IListener;
    FCancelNotifier: INotifierOperationInternal;
    FProgressInfo: IMergePolygonsProgress;
    procedure OnTimer;
    procedure OnClose;
    procedure CancelOperation;
  public
    constructor Create(
      AOwner: TComponent;
      const ATimerTickInterval: Cardinal;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const ACancelNotifier: INotifierOperationInternal;
      const AProgressInfo: IMergePolygonsProgress
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerTime,
  u_ListenerByEvent;

resourcestring
  rsElapsedTime = 'Elapsed time: %s';

{$R *.dfm}

constructor TfrmMergePolygonsProgress.Create(
  AOwner: TComponent;
  const ATimerTickInterval: Cardinal;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const ACancelNotifier: INotifierOperationInternal;
  const AProgressInfo: IMergePolygonsProgress
);
begin
  inherited Create(AOwner);

  FCancelNotifier := ACancelNotifier;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
  FProgressInfo := AProgressInfo;

  FLastOnTimer := 0;

  FTimerListener := TListenerTimeCheck.Create(Self.OnTimer, ATimerTickInterval div 2);
  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnClose);

  FTimerNoifier.Add(FTimerListener);

  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnClose;
  end;

  FCancelListener := TNotifyNoMmgEventListener.Create(Self.OnClose);
  FCancelNotifier.AddListener(FCancelListener);
end;

destructor TfrmMergePolygonsProgress.Destroy;
begin
  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingNotifier := nil;
    FAppClosingListener := nil;
  end;

  if Assigned(FTimerNoifier) and Assigned(FTimerListener) then begin
    FTimerNoifier.Remove(FTimerListener);
    FTimerNoifier := nil;
    FTimerListener := nil;
  end;

  if Assigned(FCancelNotifier) and Assigned(FCancelListener) then begin
    FCancelNotifier.RemoveListener(FCancelListener);
    FCancelNotifier := nil;
    FCancelListener := nil;
  end;

  inherited Destroy;
end;

procedure TfrmMergePolygonsProgress.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  CancelOperation;
  Action := caHide;
  Application.MainForm.SetFocus;
end;

procedure TfrmMergePolygonsProgress.btnAbortClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMergePolygonsProgress.OnTimer;
begin
  if FProgressInfo <> nil then begin
    if FProgressInfo.IsFinished then begin
      Close;
    end else begin
      if (GetTickCount - FLastOnTimer) >= 150 then begin
        lblProgress.Caption :=
          Format(rsElapsedTime, [FormatDateTime('hh:nn:ss', Now - FProgressInfo.StartedAt)]);
        FLastOnTimer := GetTickCount;
      end;
    end;
  end;
end;

procedure TfrmMergePolygonsProgress.OnClose;
begin
  Close;
end;

procedure TfrmMergePolygonsProgress.CancelOperation;
begin
  if Assigned(FCancelNotifier) and not FProgressInfo.IsFinished then begin
    FProgressInfo.IsFinished := True;
    FCancelNotifier.NextOperation;
  end;
end;

end.
