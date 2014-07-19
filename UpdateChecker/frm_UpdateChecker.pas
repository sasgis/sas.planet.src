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

unit frm_UpdateChecker;

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
  ComCtrls,
  ExtCtrls,
  i_Listener,
  i_PathConfig,
  i_LanguageManager,
  i_NotifierOperation,
  i_UpdateDownloader,
  i_BuildInfo,
  i_InetConfig,
  u_CommonFormAndFrameParents;

type
  TfrmUpdateChecker = class(TFormWitghLanguageManager)
    lblCurVer: TLabel;
    lblNewVer: TLabel;
    pbDownloadProgress: TProgressBar;
    lblProgressInfo: TLabel;
    btnDownload: TButton;
    tmrCheckState: TTimer;
    lblCurVerValue: TLabel;
    lblNewVerValue: TLabel;
    btnClose: TButton;
    cbbChannel: TComboBox;
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure tmrCheckStateTimer(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
    procedure cbbChannelChange(Sender: TObject);
  private
    FCurDate: TDateTime;
    FCurRevision: Integer;
    FNewDate: TDateTime;
    FNewRevision: Integer;
    FNewBuildType: string;
    FSearchProgress: Byte;
    FSearchProgressLastStep: Cardinal;
    FUpdateDownloader: IUpdateDownloader;
    FState: TUpdateDownloaderState;
    FAppClosingNotifier: INotifierOneOperation;
    FAppClosingListener: IListener;
    FCancelNotifierInternal: INotifierOperationInternal;
    FDownloadProgressDefCaption: string;
    FNewVerValueDefColor: TColor;
    procedure OnAppClosing;
    procedure CancelOperation;
    procedure CheckAvailableVersion;
    procedure OnVersionCheckError(const AMsg: string);
    procedure ShowError(const AMsg: string);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AUpdatesPath: IPathConfig;
      const ABuildInfo: IBuildInfo;
      const AInetConfig: IInetConfig;
      const AAppClosingNotifier: INotifierOneOperation
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  u_Notifier,
  u_NotifierOperation,
  u_ListenerByEvent,
  u_UpdateDownloader,
  u_Synchronizer,
  u_ResStrings;

resourcestring
  rsError = 'Error';
  rsSearchAvailableVersionInfo = 'searching';
  rsDownloadProgressInfo = '%s %.2f of %.2f MB (%.2f%s)';
  rsDownloadFinished = 'Download completed successfully. You can update the program manually from file: %s';

const
  cSearchAvailableVersionInfoProgressStep: Cardinal = 500; // ms

{ TfrmUpdateChecker }

constructor TfrmUpdateChecker.Create(
  const ALanguageManager: ILanguageManager;
  const AUpdatesPath: IPathConfig;
  const ABuildInfo: IBuildInfo;
  const AInetConfig: IInetConfig;
  const AAppClosingNotifier: INotifierOneOperation
);
var
  VTmp: string;
begin
  Assert(AUpdatesPath <> nil);
  Assert(ABuildInfo <> nil);
  Assert(AAppClosingNotifier <> nil);

  inherited Create(ALanguageManager);
  FAppClosingNotifier := AAppClosingNotifier;

  FCancelNotifierInternal :=
    TNotifierOperation.Create(
      TNotifierBase.Create(GSync.SyncVariable.Make(Self.ClassName + 'Notifier'))
    );

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    Self.OnAppClosing;
  end;

  FCurDate := ABuildInfo.GetBuildDate;
  if not ABuildInfo.GetBuildSrcInfo(FCurRevision, VTmp) then begin
    FCurRevision := 0;
  end;
  lblCurVerValue.Caption := ABuildInfo.GetVersion + ' ' + ABuildInfo.GetBuildType;

  cbbChannel.ItemIndex := 0; // Nightly channel

  FUpdateDownloader := TUpdateDownloader.Create(
    AUpdatesPath.FullPath,
    TUpdateChannel(cbbChannel.ItemIndex),
    AInetConfig,
    FCancelNotifierInternal
  );

  FDownloadProgressDefCaption := lblProgressInfo.Caption;
  FNewVerValueDefColor := lblNewVerValue.Font.Color;
end;

destructor TfrmUpdateChecker.Destroy;
begin
  if Assigned(tmrCheckState) then begin
    tmrCheckState.Enabled := False;
  end;
  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingNotifier := nil;
    FAppClosingListener := nil;
  end;
  FCancelNotifierInternal := nil;

  inherited;
end;

procedure TfrmUpdateChecker.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  tmrCheckState.Enabled := False;
  Self.CancelOperation;
  Action := caHide;
  Application.MainForm.SetFocus;
end;

procedure TfrmUpdateChecker.FormShow(Sender: TObject);
begin
  CheckAvailableVersion;
end;

procedure TfrmUpdateChecker.btnCloseClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmUpdateChecker.OnAppClosing;
begin
  Self.Close;
end;

procedure TfrmUpdateChecker.CheckAvailableVersion;
var
  VOperation: Integer;
begin
  VOperation := FCancelNotifierInternal.CurrentOperation;

  // reset controls
  cbbChannel.Enabled := True;
  tmrCheckState.Enabled := False;
  btnDownload.Enabled := False;
  lblNewVerValue.Font.Color := FNewVerValueDefColor;
  lblNewVerValue.Font.Style := [];
  lblProgressInfo.Caption := FDownloadProgressDefCaption;
  pbDownloadProgress.Position := 0;
  pbDownloadProgress.Enabled := False;

  FSearchProgressLastStep := GetTickCount;
  FSearchProgress := 0;
  lblNewVerValue.Caption := rsSearchAvailableVersionInfo + '...';

  // start search avilable version
  FState := FUpdateDownloader.SearchAvailableVersionInfoAsync(VOperation);

  if FState = udsSearch then begin
    tmrCheckState.Enabled := True;
  end else begin
    OnVersionCheckError(FUpdateDownloader.GetError);
  end;
end;

procedure TfrmUpdateChecker.btnDownloadClick(Sender: TObject);
var
  VMsg: string;
  VFileName: string;
  VOperation: Integer;
begin
  cbbChannel.Enabled := False;

  VFileName := FUpdateDownloader.GetFileName;
  if FileExists(VFileName) then begin
    VMsg := Format(SAS_MSG_FileExists, [VFileName]);
    if (Application.MessageBox(PChar(VMsg), PChar(SAS_MSG_coution), 36) <> IDYES) then begin
      Self.Close;
    end;
  end;

  VOperation := FCancelNotifierInternal.CurrentOperation;

  // start downloader
  FState := FUpdateDownloader.DownloadAvailableVersionAsync(VOperation);

  if FState = udsDownload then begin
    tmrCheckState.Enabled := True;
  end else begin
    ShowError(FUpdateDownloader.GetError);
  end;
end;

procedure TfrmUpdateChecker.CancelOperation;
begin
  if Assigned(FCancelNotifierInternal) then begin
    FCancelNotifierInternal.NextOperation;
  end;
end;

procedure TfrmUpdateChecker.cbbChannelChange(Sender: TObject);
begin
  if Assigned(FCancelNotifierInternal) then begin
    FCancelNotifierInternal.NextOperation;
  end;
  if Assigned(FUpdateDownloader) then begin
    FUpdateDownloader.SetUpdateChannel(TUpdateChannel(cbbChannel.ItemIndex));
    CheckAvailableVersion;
  end;
end;

procedure TfrmUpdateChecker.tmrCheckStateTimer(Sender: TObject);
var
  VDone, VTotal: Integer;
  VPercent: Single;
  VDots: string;
  I: Integer;
begin
  case FState of

    udsSearch: begin
      FState := FUpdateDownloader.GetState;
      case FState of
        udsIdle: begin
          if FUpdateDownloader.GetAvailableVersionInfo(FNewDate, FNewRevision, FNewBuildType) then begin
            lblNewVerValue.Caption :=
              FormatDateTime('yymmdd', FNewDate) + '.' + IntToStr(FNewRevision) + ' ' + FNewBuildType;
            if (FNewDate > FCurDate) or (FNewRevision > FCurRevision) then begin
              btnDownload.Enabled := True;
              lblNewVerValue.Font.Color := clGreen;
              lblNewVerValue.Font.Style := [fsBold];
            end else begin
              lblNewVerValue.Font.Color := clGray;
            end;
          end else begin
            FState := udsError;
            OnVersionCheckError(FUpdateDownloader.GetError);
          end;
        end;
        udsError: begin
          OnVersionCheckError(FUpdateDownloader.GetError);
        end;
        udsSearch: begin
          // indicate serch progress
          if GetTickCount - FSearchProgressLastStep > cSearchAvailableVersionInfoProgressStep then begin
            FSearchProgressLastStep := GetTickCount;
            FSearchProgress := (FSearchProgress + 1) mod 4;
            VDots := '';
            for I := 0 to FSearchProgress - 1 do begin
              VDots := VDots + '.';
            end;
            lblNewVerValue.Caption := rsSearchAvailableVersionInfo + VDots;
          end;
        end;
      else
        Assert(False);
      end;
    end;

    udsDownload: begin
      FState := FUpdateDownloader.GetState;
      case FState of
        udsIdle: begin
          // all done!
          cbbChannel.Enabled := True;
          pbDownloadProgress.Position := pbDownloadProgress.Max;
          lblProgressInfo.Caption := FDownloadProgressDefCaption;
          MessageDlg(Format(rsDownloadFinished, [FUpdateDownloader.GetFileName]), mtInformation, [mbOK], 0);
          Self.Close;
        end;
        udsError: begin
          ShowError(FUpdateDownloader.GetError);
        end;
        udsDownload: begin
          if not FUpdateDownloader.GetDownloadProgress(VDone, VTotal) then begin
            FState := udsError;
            ShowError(FUpdateDownloader.GetError);
          end else begin
            // show download progress
            if VTotal > 0 then begin
              if VDone = VTotal then begin
                VPercent := 100;
              end else begin
                VPercent := (VDone / VTotal) * 100;
              end;
            end else begin
              VPercent := 0.0;
            end;

            pbDownloadProgress.Max := VTotal;
            pbDownloadProgress.Position := VDone;
            lblProgressInfo.Caption :=
              Format(
                rsDownloadProgressInfo,
                [
                  FDownloadProgressDefCaption,
                  VDone / 1024 / 1024,
                  VTotal / 1024 / 1024,
                  VPercent,
                  '%'
                ]
              );
          end;
        end;
      else
        Assert(False);
      end;
    end;

    udsIdle, udsError: begin
      tmrCheckState.Enabled := False;
    end;
  end;
end;

procedure TfrmUpdateChecker.OnVersionCheckError(const AMsg: string);
begin
  lblNewVerValue.Caption := rsError + '!';
  lblNewVerValue.Font.Color := clRed;
  lblNewVerValue.Font.Style := [fsBold];
  ShowError(AMsg);
end;

procedure TfrmUpdateChecker.ShowError(const AMsg: string);
begin
  MessageDlg(AMsg, mtError, [mbOK], 0);
end;

end.
