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

unit frm_ProgressCacheConvrter;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  i_NotifierTime,
  i_Listener,
  i_ListenerTime,
  i_CacheConverterProgressInfo,
  i_LanguageManager,
  i_ValueToStringConverter,
  i_NotifierOperation,
  u_ThreadCacheConverter,
  u_CommonFormAndFrameParents;

type
  TfrmProgressCacheConverter = class(TFormWitghLanguageManager)
    pnlBottom: TPanel;
    btnAbort: TButton;
    btnPause: TButton;
    btnMinimize: TButton;
    lblProcessedName: TLabel;
    lblSkippedName: TLabel;
    lblSizeName: TLabel;
    lblLastTileName: TLabel;
    lblLastTileValue: TLabel;
    lblSizeValue: TLabel;
    lblSkippedValue: TLabel;
    lblProcessedValue: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(
      Sender: TObject;
      var Key: Word;
      Shift: TShiftState
    );
    procedure FormClose(
      Sender: TObject;
      var Action: TCloseAction
    );
    procedure btnMinimizeClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
  private
    FConverterThread: TThreadCacheConverter;
    FAppClosingNotifier: INotifierOneOperation;
    FAppClosingListener: IListener;
    FTimerNoifier: INotifierTime;
    FTimerListener: IListenerTime;
    FCancelNotifierInternal: INotifierOperationInternal;
    FProgressInfo: ICacheConverterProgressInfo;
    FValueToStringConverter: IValueToStringConverterChangeable;
    FFinished: Boolean;
    procedure OnAppClosing;
    procedure CancelOperation;
    procedure OnTimerTick;
  public
    constructor Create(
      const AConverterThread: TThreadCacheConverter;
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const ACancelNotifierInternal: INotifierOperationInternal;
      const AProgressInfo: ICacheConverterProgressInfo;
      const AValueToStringConverter: IValueToStringConverterChangeable
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  u_Dialogs,
  u_ListenerByEvent,
  u_ListenerTime,
  u_ResStrings;

{$R *.dfm}

{ TfrmProgressCacheConverter }

constructor TfrmProgressCacheConverter.Create(
  const AConverterThread: TThreadCacheConverter;
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const ACancelNotifierInternal: INotifierOperationInternal;
  const AProgressInfo: ICacheConverterProgressInfo;
  const AValueToStringConverter: IValueToStringConverterChangeable
);
begin
  inherited Create(ALanguageManager);
  FConverterThread := AConverterThread;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
  FCancelNotifierInternal := ACancelNotifierInternal;
  FProgressInfo := AProgressInfo;
  FValueToStringConverter := AValueToStringConverter;

  FTimerListener := TListenerTimeCheck.Create(Self.OnTimerTick, 250);
  FTimerNoifier.Add(FTimerListener);

  FFinished := False;

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnAppClosing;
  end;
end;

destructor TfrmProgressCacheConverter.Destroy;
begin
  if Assigned(FTimerNoifier) and Assigned(FTimerListener) then begin
    FTimerNoifier.Remove(FTimerListener);
    FTimerNoifier := nil;
    FTimerListener := nil;
  end;
  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingNotifier := nil;
    FAppClosingListener := nil;
  end;
  inherited;
end;

procedure TfrmProgressCacheConverter.FormCreate(Sender: TObject);
begin
  Self.Show;
end;

procedure TfrmProgressCacheConverter.FormClose(
  Sender: TObject;
  var Action: TCloseAction
);
begin
  CancelOperation;
  Action := caFree;
  Application.MainForm.SetFocus;
end;

procedure TfrmProgressCacheConverter.FormKeyUp(
  Sender: TObject;
  var Key: Word;
  Shift: TShiftState
);
begin
  if Key = VK_ESCAPE then begin
    Self.Close;
  end;
end;

procedure TfrmProgressCacheConverter.OnAppClosing;
begin
  Self.Close;
end;

procedure TfrmProgressCacheConverter.btnMinimizeClick(Sender: TObject);
begin
  Self.WindowState := wsMinimized;
end;

procedure TfrmProgressCacheConverter.btnPauseClick(Sender: TObject);
begin
  if not FFinished then begin
    if FConverterThread.Paused then begin
      FConverterThread.Paused := False;
      btnPause.Caption := SAS_STR_Pause;
    end else begin
      FConverterThread.Paused := True;
      btnPause.Caption := SAS_STR_Continue;
    end;
  end;
end;

procedure TfrmProgressCacheConverter.btnAbortClick(Sender: TObject);
begin
  FFinished := True;
  CancelOperation;
  Application.ProcessMessages;
  Self.Close;
end;

procedure TfrmProgressCacheConverter.CancelOperation;
begin
  if FCancelNotifierInternal <> nil then begin
    FCancelNotifierInternal.NextOperation;
  end;
end;

procedure TfrmProgressCacheConverter.OnTimerTick;
var
  VValueConverter: IValueToStringConverter;
begin
  if (FProgressInfo <> nil) and (not FFinished) then begin
    VValueConverter := FValueToStringConverter.GetStatic;
    lblProcessedValue.Caption := FloatToStrF(FProgressInfo.TilesProcessed, ffNumber, 12, 0);
    lblSkippedValue.Caption := FloatToStrF(FProgressInfo.TilesSkipped, ffNumber, 12, 0);
    lblSizeValue.Caption := VValueConverter.DataSizeConvert(FProgressInfo.TilesSize / 1024);
    lblLastTileValue.Caption := FProgressInfo.LastTileName;
    if FProgressInfo.Finished then begin
      FFinished := True;
      btnPause.Visible := False;
      btnAbort.Caption := _('Close');
      Self.Caption := SAS_STR_Finished;
      if FProgressInfo.ProgressAbortErrorStr <> '' then begin
        ShowErrorMessage(FProgressInfo.ProgressAbortErrorStr);
      end else begin
        ShowInfoMessage(SAS_STR_CacheConvertionIsFinished);
      end;
    end;
  end;
end;

end.
