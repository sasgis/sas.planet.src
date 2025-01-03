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
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  t_UpdateChecker,
  i_PathConfig,
  i_LanguageManager,
  i_BuildInfo,
  i_InetConfig,
  i_DownloaderFactory,
  i_UpdateProgress,
  u_CommonFormAndFrameParents;

type
  TfrmUpdateChecker = class(TFormWitghLanguageManager)
    lblCurVer: TLabel;
    lblNewVer: TLabel;
    pbDownloadProgress: TProgressBar;
    lblProgressInfo: TLabel;
    btnDownload: TButton;
    tmrProgress: TTimer;
    lblCurVerValue: TLabel;
    lblNewVerValue: TLabel;
    btnClose: TButton;
    cbbChannel: TComboBox;
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(
      Sender: TObject;
      var Action: TCloseAction
    );
    procedure FormShow(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
    procedure cbbChannelChange(Sender: TObject);
  private
    FCurDate: TDateTime;
    FCurRevision: Integer;
    FCurBuildType: string;

    FDownloadProgressDefCaption: string;
    FNewVerValueDefColor: TColor;
    FBuildInfo: IBuildInfo;

    FSearchProgress: Byte;
    FSearchProgressLastStep: Cardinal;

    FUpdateCheckerOperationID: Integer;
    FUpdateCheckerProgress: IUpdateCheckerProgress;
    FUpdateCheckerResult: TUpdateCheckerResult;

    FUpdateDownloaderOperationID: Integer;
    FUpdateDownloaderProgress: IUpdateDownloaderProgress;

    FUpdatesPath: IPathConfig;
    FInetConfig: IInetConfig;
    FDownloaderFactory: IDownloaderFactory;

    procedure ResetTimer;
    procedure OnTimerCheckerProgress(Sender: TObject);
    procedure OnTimerDownloaderProgress(Sender: TObject);

    procedure CancelOperations;
    procedure CheckAvailableVersion;
    function GetCurVersionStr: string;
  protected
    procedure RefreshTranslation; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AUpdatesPath: IPathConfig;
      const ABuildInfo: IBuildInfo;
      const AInetConfig: IInetConfig;
      const ADownloaderFactory: IDownloaderFactory
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  u_Dialogs,
  u_UpdateDownloaderThread,
  u_UpdateCheckerThread,
  u_UpdateProgress;

resourcestring
  rsError = 'Error';
  rsSearchAvailableVersionInfo = 'searching';
  rsDownloadProgressInfo = '%s %.2f of %.2f MB (%.2f%s)';
  rsDownloadFinished = 'Download completed successfully. You can update the program manually from file: %s';

{ TfrmUpdateChecker }

constructor TfrmUpdateChecker.Create(
  const ALanguageManager: ILanguageManager;
  const AUpdatesPath: IPathConfig;
  const ABuildInfo: IBuildInfo;
  const AInetConfig: IInetConfig;
  const ADownloaderFactory: IDownloaderFactory
);
var
  VTmp: string;
begin
  inherited Create(ALanguageManager);

  FUpdatesPath := AUpdatesPath;
  FBuildInfo := ABuildInfo;
  FInetConfig := AInetConfig;
  FDownloaderFactory := ADownloaderFactory;

  FUpdateCheckerProgress := TUpdateCheckerProgress.Create;
  FUpdateDownloaderProgress := TUpdateDownloaderProgress.Create;

  FCurDate := FBuildInfo.GetBuildDate;
  FCurBuildType := FBuildInfo.GetBuildType;
  if not FBuildInfo.GetBuildSrcInfo(FCurRevision, VTmp) then begin
    FCurRevision := 0;
  end;
  lblCurVerValue.Caption := Self.GetCurVersionStr;

  cbbChannel.ItemIndex := 0; // Nightly channel

  FDownloadProgressDefCaption := lblProgressInfo.Caption;
  FNewVerValueDefColor := lblNewVerValue.Font.Color;

  ResetTimer;
end;

destructor TfrmUpdateChecker.Destroy;
begin
  ResetTimer;

  if Assigned(FUpdateCheckerProgress) then begin
    FUpdateCheckerProgress.Reset;
  end;

  if Assigned(FUpdateDownloaderProgress) then begin
    FUpdateDownloaderProgress.Reset;
  end;

  inherited Destroy;
end;

procedure TfrmUpdateChecker.FormClose(
  Sender: TObject;
  var Action: TCloseAction
);
begin
  CancelOperations;

  Action := caHide;
  Application.MainForm.SetFocus;
end;

procedure TfrmUpdateChecker.FormShow(Sender: TObject);
begin
  CheckAvailableVersion;
end;

function TfrmUpdateChecker.GetCurVersionStr: string;
begin
  Result := FBuildInfo.GetVersion + ' ' + FCurBuildType;
end;

procedure TfrmUpdateChecker.cbbChannelChange(Sender: TObject);
begin
  CheckAvailableVersion;
end;

procedure TfrmUpdateChecker.btnCloseClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmUpdateChecker.CancelOperations;
begin
  ResetTimer;

  FUpdateCheckerProgress.Reset;
  FUpdateDownloaderProgress.Reset;
end;

procedure TfrmUpdateChecker.CheckAvailableVersion;
begin
  CancelOperations;

  cbbChannel.Enabled := True;

  btnDownload.Enabled := False;

  lblProgressInfo.Visible := False;

  pbDownloadProgress.Position := 0;
  pbDownloadProgress.Enabled := False;

  lblNewVerValue.Font.Color := FNewVerValueDefColor;
  lblNewVerValue.Font.Style := [];
  lblNewVerValue.Caption := rsSearchAvailableVersionInfo + '...';

  FSearchProgressLastStep := GetTickCount;
  FSearchProgress := 0;

  // start update checker thread
  FUpdateCheckerOperationID := FUpdateCheckerProgress.CurrentOperationID;

  TUpdateCheckerThread.Create(
    TUpdateChannel(cbbChannel.ItemIndex),
    FDownloaderFactory,
    FInetConfig,
    FUpdateCheckerProgress
  );

  tmrProgress.OnTimer := Self.OnTimerCheckerProgress;
  tmrProgress.Enabled := True;
end;

procedure TfrmUpdateChecker.btnDownloadClick(Sender: TObject);
begin
  CancelOperations;

  cbbChannel.Enabled := False;
  btnDownload.Enabled := False;

  lblProgressInfo.Caption := FDownloadProgressDefCaption;
  lblProgressInfo.Visible := True;

  // start update downloader thread
  FUpdateDownloaderOperationID := FUpdateDownloaderProgress.CurrentOperationID;

  TUpdateDownloaderThread.Create(
    FUpdateCheckerResult,
    FUpdatesPath,
    FDownloaderFactory,
    FInetConfig,
    FUpdateDownloaderProgress
  );

  tmrProgress.OnTimer := Self.OnTimerDownloaderProgress;
  tmrProgress.Enabled := True;
end;

procedure TfrmUpdateChecker.OnTimerCheckerProgress(Sender: TObject);
const
  CUpdateCheckerProgressStep: Cardinal = 300; // ms
var
  I: Integer;
  VDots: string;
  VRevision: string;
  VIsNewRevision: Boolean;
  VStatus: TUpdateProgressStatus;
  VResult: ^TUpdateCheckerResult;
begin
  VResult := @FUpdateCheckerResult;
  VStatus := FUpdateCheckerProgress.GetResult(FUpdateCheckerOperationID, FUpdateCheckerResult);

  case VStatus of
    psBusy: begin
      if GetTickCount - FSearchProgressLastStep > CUpdateCheckerProgressStep then begin
        FSearchProgressLastStep := GetTickCount;
        FSearchProgress := (FSearchProgress + 1) mod 4;
        VDots := '';
        for I := 0 to FSearchProgress - 1 do begin
          VDots := VDots + '.';
        end;
        lblNewVerValue.Caption := rsSearchAvailableVersionInfo + VDots;
      end;
    end;

    psFinished: begin
      ResetTimer;

      if not VResult.IsFound then begin
        lblNewVerValue.Caption := rsError + '!';
        lblNewVerValue.Font.Color := clRed;
        lblNewVerValue.Font.Style := [fsBold];

        ShowErrorMessage('Update check failed!');
        Exit;
      end;

      VRevision := '';
      if VResult.BuildRevision > 0 then begin
        VRevision := '.' + IntToStr(VResult.BuildRevision);
      end;

      lblNewVerValue.Caption :=
        FormatDateTime('yymmdd', VResult.BuildDate) +
        VRevision + ' ' +
        VResult.BuildType;

      VIsNewRevision :=
        (VResult.BuildRevision > 0) and
        (FCurRevision > 0) and
        (VResult.BuildRevision > FCurRevision);

      if
        VIsNewRevision or
        (VResult.BuildDate > FCurDate) or
        (VResult.BuildType <> FCurBuildType)
      then begin
        btnDownload.Enabled := True;
        lblNewVerValue.Font.Color := clGreen;
        lblNewVerValue.Font.Style := [fsBold];
      end else begin
        lblNewVerValue.Font.Color := clGray;
      end;
    end;

    psCanceled: begin
      // nothing to do
    end;
  else
    raise Exception.CreateFmt(
      'Unexpected TUpdateProgressStatus value: %d', [Integer(VStatus)]
    );
  end;
end;

procedure TfrmUpdateChecker.OnTimerDownloaderProgress(Sender: TObject);

  procedure UpdateProgress(const AResult: TUpdateDownloaderResult);
  var
    VPercent: Double;
  begin
    if AResult.BytesTotal > 0 then begin
      if AResult.BytesDownloaded = AResult.BytesTotal then begin
        VPercent := 100;
      end else begin
        VPercent := 100 * AResult.BytesDownloaded / AResult.BytesTotal;
      end;
    end else begin
      VPercent := 0;
    end;

    pbDownloadProgress.Max := 100;
    pbDownloadProgress.Position := Round(VPercent);

    lblProgressInfo.Caption := Format(
      rsDownloadProgressInfo,
      [FDownloadProgressDefCaption, AResult.BytesDownloaded / 1024 / 1024,
      AResult.BytesTotal / 1024 / 1024,  VPercent, '%']
    );
  end;

var
  VStatus: TUpdateProgressStatus;
  VResult: TUpdateDownloaderResult;
begin
  VStatus := FUpdateDownloaderProgress.GetResult(FUpdateDownloaderOperationID, VResult);

  case VStatus of
    psBusy: begin
      UpdateProgress(VResult);
    end;

    psFinished: begin
      ResetTimer;

      UpdateProgress(VResult);

      if not VResult.IsError then begin
        ShowInfoMessage(Format(rsDownloadFinished, [VResult.Text]));
      end else begin
        ShowErrorMessage(VResult.Text);
      end;

      Self.Close;
    end;

    psCanceled: begin
      // nothing to do
    end;
  else
    raise Exception.CreateFmt(
      'Unexpected TUpdateProgressStatus value: %d', [Integer(VStatus)]
    );
  end;
end;

procedure TfrmUpdateChecker.ResetTimer;
begin
  tmrProgress.Enabled := False;
  tmrProgress.OnTimer := nil;
end;

procedure TfrmUpdateChecker.RefreshTranslation;
begin
  inherited RefreshTranslation;

  lblCurVerValue.Caption := Self.GetCurVersionStr;
end;

end.
