{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit frm_ProgressSimple;

interface

uses
  Windows,
  Classes,
  Forms,
  Graphics,
  Controls,
  StdCtrls,
  ExtCtrls,
  RarProgress,
  i_Notifier,
  i_NotifierTime,
  i_Listener,
  i_ListenerTime,
  i_RegionProcessProgressInfo,
  i_NotifierOperation,
  u_CommonFormAndFrameParents;

type
  TfrmProgressSimple = class(TCommonFormParent)
    MemoInfo: TMemo;
    pnlProgress: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MemoInfoChange(Sender: TObject);
  private
    FCancelNotifier: INotifierOperationInternal;
    FProgressInfo: IRegionProcessProgressInfo;
    FAppClosingNotifier: INotifierOneOperation;
    FTimerNoifier: INotifierTime;

    FRarProgress: TRarProgress;
    FAppClosingListener: IListener;
    FTimerListener: IListenerTime;
    procedure OnTimer;
    procedure OnClose;
    procedure CancelOperation;
  public
    constructor Create(
      AOwner : TComponent;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const ACancelNotifier: INotifierOperationInternal;
      const AProgressInfo: IRegionProcessProgressInfo
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerTime,
  u_ListenerByEvent;

{$R *.dfm}

constructor TfrmProgressSimple.Create(
  AOwner : TComponent;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const ACancelNotifier: INotifierOperationInternal;
  const AProgressInfo: IRegionProcessProgressInfo
);
begin
  inherited Create(AOwner);
  FRarProgress := TRarProgress.Create(Self);
  with FRarProgress do begin
    Left := 6;
    Top := 30;
    Width := 315;
    Height := 17;
    Min := 0;
    Max := 100;
    Progress1 := 50;
    Progress2 := 30;
    Double := False;
    LightColor1 := 16770764;
    DarkColor1 := 13395456;
    LightColor2 := 16768959;
    FrameColor1 := 16758122;
    FrameColor2 := 16747546;
    FillColor1 := 16757606;
    FillColor2 := 16749867;
    BackFrameColor1 := 16633762;
    BackFrameColor2 := 16634540;
    BackFillColor := 16635571;
    ShadowColor := clGray;
  end;
  FRarProgress.Parent := pnlProgress;
  FRarProgress.Align := alClient;
  FRarProgress.Min := 0;
  FRarProgress.Max := 100;
  FRarProgress.Progress1 := 0;
  FRarProgress.Visible := True;

  FCancelNotifier := ACancelNotifier;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
  FProgressInfo := AProgressInfo;

  FTimerListener := TListenerTimeCheck.Create(Self.OnTimer, 1000);
  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnClose);

  FTimerNoifier.Add(FTimerListener);

  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnClose;
  end;
end;

procedure TfrmProgressSimple.FormCreate(Sender: TObject);
begin
  Show;
end;

destructor TfrmProgressSimple.Destroy;
begin
  if FAppClosingNotifier <> nil then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingNotifier := nil;
    FAppClosingListener := nil;
  end;

  if FTimerNoifier <> nil then begin
    FTimerNoifier.Remove(FTimerListener);
    FTimerNoifier := nil;
    FTimerListener := nil;
  end;
  inherited;
end;

procedure TfrmProgressSimple.CancelOperation;
begin
  if FCancelNotifier <> nil then begin
    FCancelNotifier.NextOperation;
  end;
end;

procedure TfrmProgressSimple.FormClose(Sender: TObject; var Action:
    TCloseAction);
begin
  CancelOperation;
  Action := caFree;
  Application.MainForm.SetFocus;
end;

procedure TfrmProgressSimple.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then close;
end;

procedure TfrmProgressSimple.MemoInfoChange(Sender: TObject);
begin
  HideCaret(MemoInfo.Handle);
end;

procedure TfrmProgressSimple.OnClose;
begin
  Close;
end;

procedure TfrmProgressSimple.OnTimer;
begin
  if FProgressInfo <> nil then begin
    Self.Caption := FProgressInfo.Caption;
    MemoInfo.Lines[0] := FProgressInfo.FirstLine;
    MemoInfo.Lines[1] := FProgressInfo.SecondLine;
    FRarProgress.Progress1 := Trunc(FProgressInfo.ProcessedRatio * 100);
    if FProgressInfo.Finished then begin
      Close;
    end;
  end;
end;

end.



