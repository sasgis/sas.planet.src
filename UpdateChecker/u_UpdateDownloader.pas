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

unit u_UpdateDownloader;

interface

uses
  SysUtils,
  i_NotifierOperation,
  i_InetConfig,
  i_Downloader,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_UpdateDownloader,
  u_BaseInterfacedObject;

type
  TUpdateDownloader = class(TBaseInterfacedObject, IUpdateDownloader)
  private
    FState: TUpdateDownloaderState;
    FError: string;
    FDone: Integer;
    FTotal: Integer;
    FDate: TDateTime;
    FRevision: Integer;
    FBuildType: string;
    FIsFoundAvailableVersion: Boolean;
    FInetConfig: IInetConfig;
    FDownloadResultFactory: IDownloadResultFactory;
    FSearchVersionInfoUrl: string;
    FAvailableVersionUrl: string;
    FUpdateChannel: TUpdateChannel;
    FFileName: string;
    FSavePath: string;
    FCancelNotifier: INotifierOperation;
    FSync: IReadWriteSync;
  private
    procedure SearchVersionInfoCallBack(
      const AResult: IDownloadResult;
      const AOperationID: Integer
    );
    procedure DownloadAvailableVersionCallBack(
      const AResult: IDownloadResult;
      const AOperationID: Integer
    );
    procedure DownloadAvailableVersionProgressCallBack(
      const ARead: Integer;
      const ATotal: Integer
    );
  private
    { IUpdateDownloader }
    function SearchAvailableVersionInfoAsync(const AOperationID: Integer): TUpdateDownloaderState;
    function GetAvailableVersionInfo(out ADate: TDateTime; out ARev: Integer; out ABuildType: string): Boolean;
    function DownloadAvailableVersionAsync(const AOperationID: Integer): TUpdateDownloaderState;
    function GetDownloadProgress(out ADone, ATotal: Integer): Boolean;
    function GetState: TUpdateDownloaderState;
    function GetError: string;
    function GetFileName: string;
    procedure SetUpdateChannel(const AValue: TUpdateChannel);
  public
    constructor Create(
      const AOutputPath: string;
      const AUpdateChannel: TUpdateChannel;
      const AInetConfig: IInetConfig;
      const ACancelNotifier: INotifierOperation
    );
  end;

implementation

uses
  Classes,
  RegExpr,
  i_DownloadRequest,
  u_DownloadRequest,
  u_DownloaderHttp,
  u_Synchronizer,
  u_DownloadResultFactory;

const
  cNightlyChannel = 'http://sasgis.org/programs/sasplanet/nightly.php';
  cStableChannel = 'http://sasgis.org/programs/sasplanet/stable.php';

  cSearchVersionInfoRegExpr = 'SAS\.Planet\.(Stable|Release|Nightly)\.(\d\d)(\d\d)(\d\d)\.(\d+)\.(zip|rar|7z)';
  cSearchAvailableVersionUrlRegExpr = '<a href="(.*?)">' + cSearchVersionInfoRegExpr + '</a>';

{ TUpdateDownloader }

constructor TUpdateDownloader.Create(
  const AOutputPath: string;
  const AUpdateChannel: TUpdateChannel;
  const AInetConfig: IInetConfig;
  const ACancelNotifier: INotifierOperation
);
begin
  inherited Create;
  FInetConfig := AInetConfig;
  FCancelNotifier := ACancelNotifier;

  FDownloadResultFactory := TDownloadResultFactory.Create;

  FSync := MakeSyncRW_Std(Self, False);

  Self.SetUpdateChannel(AUpdateChannel);

  FAvailableVersionUrl := '';

  FDate := 0;
  FRevision := 0;
  FBuildType := '';
  FIsFoundAvailableVersion := False;

  FSavePath := IncludeTrailingPathDelimiter(AOutputPath);
end;

function TUpdateDownloader.SearchAvailableVersionInfoAsync(
  const AOperationID: Integer
): TUpdateDownloaderState;
var
  VRequest: IDownloadRequest;
  VDownloader: IDownloaderAsync;
begin
  Result := udsSearch;

  FSync.BeginWrite;
  try
    FState := Result;
    FDate := 0;
    FRevision := 0;
    FBuildType := '';
    FIsFoundAvailableVersion := False;
    FAvailableVersionUrl := '';
  finally
    FSync.EndWrite;
  end;

  VRequest := TDownloadRequest.Create(
    FSearchVersionInfoUrl,
    '',
    FInetConfig.GetStatic
  );

  VDownloader := TDownloaderHttp.Create(
    FDownloadResultFactory,
    True, // allow cookie
    False // disable redirect
  );

  VDownloader.DoRequestAsync(
    VRequest,
    FCancelNotifier,
    AOperationID,
    Self.SearchVersionInfoCallBack
  );
end;

procedure TUpdateDownloader.SearchVersionInfoCallBack(
  const AResult: IDownloadResult;
  const AOperationID: Integer
);

  function ParseVersionInfo(
    const AText: string;
    out ADate: TDateTime;
    out ARevision: Integer;
    out ABuildType: string;
    out AFullName: string
  ): string;
  var
    VRegExpr: TRegExpr;
    y, m, d: Word;
  begin
    Result := '';
    VRegExpr := TRegExpr.Create;
    try
      VRegExpr.Expression := cSearchVersionInfoRegExpr;
      if VRegExpr.Exec(AText) then begin
        AFullName := VRegExpr.Match[0];
        ABuildType := VRegExpr.Match[1];
        y := StrToIntDef('20' + VRegExpr.Match[2], 0);
        m := StrToIntDef(VRegExpr.Match[3], 0);
        d := StrToIntDef(VRegExpr.Match[4], 0);
        ADate := EncodeDate(y, m, d);
        ARevision := StrToIntDef(VRegExpr.Match[5], 0);
        if (y = 0) or (m = 0) or (d = 0) or (ADate = 0) or (ARevision = 0) then begin
          Result := 'Unexpected Date or Revision value: ' +
            VRegExpr.Match[2] + VRegExpr.Match[3] + VRegExpr.Match[4] + '.' +
            VRegExpr.Match[5] + ' (' + VRegExpr.Match[0] + ')';
        end;
      end else begin
        Result := 'HTML parser failue';
      end;
    finally
      VRegExpr.Free;
    end;
  end;

  function ParseDownloadUrl(const AText: string; out AUrl: string): string;
  var
    VRegExpr: TRegExpr;
  begin
    Result := '';
    VRegExpr := TRegExpr.Create;
    try
      VRegExpr.Expression := cSearchAvailableVersionUrlRegExpr;
      if VRegExpr.Exec(AText) then begin
        AUrl := VRegExpr.Match[1];
      end else begin
        Result := 'HTML parser failue';
      end;
    finally
      VRegExpr.Free;
    end;
  end;

var
  VError: string;
  VResponseData: AnsiString;
  VResultOk: IDownloadResultOk;
  VResultError: IDownloadResultError;
  VUrl: string;
  VName: string;
  VDate: TDateTime;
  VRevision: Integer;
  VBuildType: string;
begin
  VError := '';

  if Supports(AResult, IDownloadResultOk, VResultOk) then begin
    SetLength(VResponseData, VResultOk.Data.Size);
    Move(VResultOk.Data.Buffer^, VResponseData[1], VResultOk.Data.Size);
    VError := ParseVersionInfo(VResponseData, VDate, VRevision, VBuildType, VName);
    if VError = '' then begin
      VError := ParseDownloadUrl(VResponseData, VUrl);
    end;
  end else if Supports(AResult, IDownloadResultError, VResultError) then begin
    VError := VResultError.ErrorText;
  end else begin
    VError := 'Unexpected IDownloadResult';
  end;

  FSync.BeginWrite;
  try
    FError := VError;
    if FError = '' then begin
      FState := udsIdle;
      FIsFoundAvailableVersion := True;
      FDate := VDate;
      FRevision := VRevision;
      FBuildType := VBuildType;
      FAvailableVersionUrl := VUrl;
      FFileName := VName;
    end else begin
      FState := udsError;
    end;
  finally
    FSync.EndWrite;
  end;
end;

function TUpdateDownloader.GetAvailableVersionInfo(out ADate: TDateTime; out ARev: Integer; out ABuildType: string): Boolean;
begin
  FSync.BeginRead;
  try
    Result := FIsFoundAvailableVersion;
    ADate := FDate;
    ARev := FRevision;
    ABuildType := FBuildType;
  finally
    FSync.EndRead;
  end;
end;

function TUpdateDownloader.DownloadAvailableVersionAsync(
  const AOperationID: Integer
): TUpdateDownloaderState;
var
  VUrl: string;
  VRequest: IDownloadRequest;
  VDownloader: IDownloaderAsync;
begin
  FSync.BeginWrite;
  try
    if FState in [udsError, udsSearch] then begin
      Result := FState;
      Exit;
    end;
    FState := udsDownload;
    VUrl := FAvailableVersionUrl;
    Result := FState;
  finally
    FSync.EndWrite;
  end;

  VRequest := TDownloadRequest.Create(
    VUrl,
    '',
    FInetConfig.GetStatic
  );

  VDownloader := TDownloaderHttp.Create(
    FDownloadResultFactory,
    True, // allow cookie
    True, // allow redirect
    Self.DownloadAvailableVersionProgressCallBack
  );

  VDownloader.DoRequestAsync(
    VRequest,
    FCancelNotifier,
    AOperationID,
    Self.DownloadAvailableVersionCallBack
  );
end;

procedure TUpdateDownloader.DownloadAvailableVersionCallBack(
  const AResult: IDownloadResult;
  const AOperationID: Integer
);
var
  VError: string;
  VFilePath: string;
  VMemStream: TMemoryStream;
  VResultOk: IDownloadResultOk;
  VResultError: IDownloadResultError;
begin
  if Supports(AResult, IDownloadResultOk, VResultOk) then begin
    VFilePath := Self.GetFileName;
    try
      if ForceDirectories(ExtractFilePath(VFilePath)) then begin
        VMemStream := TMemoryStream.Create;
        try
          VMemStream.WriteBuffer(VResultOk.Data.Buffer^, VResultOk.Data.Size);
          VMemStream.SaveToFile(VFilePath);
        finally
          VMemStream.Free;
        end;
      end else begin
        VError := SysErrorMessage(GetLastError);
      end;
    except
      on E: Exception do begin
        VError := E.ClassName + ': ' + E.Message;
      end;
    end;
  end else if Supports(AResult, IDownloadResultError, VResultError) then begin
    VError := VResultError.ErrorText;
  end else begin
    VError := 'Unexpected IDownloadResult';
  end;

  FSync.BeginWrite;
  try
    FError := VError;
    if FError = '' then begin
      FState := udsIdle;
    end else begin
      FState := udsError;
    end;
  finally
    FSync.EndWrite;
  end;
end;

procedure TUpdateDownloader.DownloadAvailableVersionProgressCallBack(
  const ARead: Integer;
  const ATotal: Integer
);
begin
  FSync.BeginWrite;
  try
    FDone := ARead;
    FTotal := ATotal;
  finally
    FSync.EndWrite;
  end;
end;

function TUpdateDownloader.GetDownloadProgress(out ADone, ATotal: Integer): Boolean;
begin
  FSync.BeginRead;
  try
    Result := FIsFoundAvailableVersion and (FState in [udsDownload, udsIdle]);
    ADone := FDone;
    ATotal := FTotal;
  finally
    FSync.EndRead;
  end;
end;

function TUpdateDownloader.GetState: TUpdateDownloaderState;
begin
  FSync.BeginRead;
  try
    Result := FState;
  finally
    FSync.EndRead;
  end;
end;

function TUpdateDownloader.GetError: string;
begin
  FSync.BeginRead;
  try
    Result := FError;
  finally
    FSync.EndRead;
  end;
end;

function TUpdateDownloader.GetFileName: string;
begin
  FSync.BeginRead;
  try
    Result := FSavePath + FFileName;
  finally
    FSync.EndRead;
  end;
end;

procedure TUpdateDownloader.SetUpdateChannel(const AValue: TUpdateChannel);
begin
  FSync.BeginWrite;
  try
    FUpdateChannel := AValue;
    case FUpdateChannel of
      ucNightly: FSearchVersionInfoUrl := cNightlyChannel;
      ucStable: FSearchVersionInfoUrl := cStableChannel;
    else
      Assert(False);
    end;
  finally
    FSync.EndWrite;
  end;
end;

end.
