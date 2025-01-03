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

unit frm_ElevationMetaWriterProgress;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  i_Listener,
  i_NotifierOperation,
  i_ElevationMetaWriterProgress,
  u_CommonFormAndFrameParents;

type
  TfrmElevationMetaWriterProgress = class(TCommonFormParent)
    btnCancel: TButton;
    lblProgress: TLabel;
    tmrProgress: TTimer;
    procedure FormClose(
      Sender: TObject;
      var Action: TCloseAction
    );
    procedure btnCancelClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    FAppClosingNotifier: INotifierOneOperation;
    FAppClosingListener: IListener;
    FProgress: IElevationMetaWriterProgress;

    procedure OnTimer(Sender: TObject);
    procedure OnClose;
  public
    procedure ShowProgress;

    constructor Create(
      const AAppClosingNotifier: INotifierOneOperation;
      const AProgress: IElevationMetaWriterProgress
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent;

resourcestring
  rsInitializing = 'Initializing...';
  rsProcessedFmt = 'Processed: %.2f%%';

{$R *.dfm}

constructor TfrmElevationMetaWriterProgress.Create(
  const AAppClosingNotifier: INotifierOneOperation;
  const AProgress: IElevationMetaWriterProgress
);
begin
  Assert(AProgress <> nil);

  inherited Create(nil);

  FAppClosingNotifier := AAppClosingNotifier;
  FProgress := AProgress;

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnClose);
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnClose;
  end;

  tmrProgress.OnTimer := Self.OnTimer;
end;

destructor TfrmElevationMetaWriterProgress.Destroy;
begin
  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingNotifier := nil;
    FAppClosingListener := nil;
  end;

  inherited Destroy;
end;

procedure TfrmElevationMetaWriterProgress.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FProgress.Status := emwCanceled;
  tmrProgress.Enabled := False;
  Action := caHide;
end;

procedure TfrmElevationMetaWriterProgress.FormHide(Sender: TObject);
begin
  tmrProgress.Enabled := False;
end;

procedure TfrmElevationMetaWriterProgress.OnClose;
begin
  Close;
end;

procedure TfrmElevationMetaWriterProgress.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmElevationMetaWriterProgress.ShowProgress;
begin
  lblProgress.Caption := rsInitializing;
  tmrProgress.Enabled := True;
  Self.Show;
end;

procedure TfrmElevationMetaWriterProgress.OnTimer(Sender: TObject);
var
  VInfo: TElevationMetaWriterProgressInfo;
begin
  VInfo := FProgress.GetInfo;

  if (FProgress.Status = emwIdle) or (VInfo.TotalCount <= 0) then begin
    Exit;
  end;

  lblProgress.Caption := Format(rsProcessedFmt, [VInfo.ReadyCount / VInfo.TotalCount * 100]);
end;

end.
