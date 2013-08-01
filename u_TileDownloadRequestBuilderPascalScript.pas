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

unit u_TileDownloadRequestBuilderPascalScript;

interface

uses
  Windows,
  SysUtils,
  uPSRuntime,
  uPSUtils,
  i_Notifier,
  i_Listener,
  i_CoordConverter,
  i_NotifierOperation,
  i_TileDownloaderConfig,
  i_TileRequest,
  i_Downloader,
  i_DownloadChecker,
  i_LanguageManager,
  i_LastResponseInfo,
  i_ProjConverter,
  i_SimpleFlag,
  i_TileDownloadRequest,
  i_TileDownloadRequestBuilderConfig,
  u_BasePascalCompiler,
  u_TileDownloadRequestBuilder;

type
  TTileDownloadRequestBuilderPascalScript = class(TTileDownloadRequestBuilder)
  private
    FTileDownloaderConfig: ITileDownloaderConfig;
    FCheker: IDownloadChecker;
    FDownloader: IDownloader;
    FCoordConverter: ICoordConverterSimple;
    FDefProjConverter: IProjConverter;
    FProjFactory: IProjConverterFactory;
    FScriptBuffer: AnsiString;

    FLang: AnsiString;
    FLangManager: ILanguageManager;
    FLangListener: IListener;
    FLangChangeFlag: ISimpleFlag;

    FPSExec: TBasePascalScriptExec;
    FpResultUrl: PPSVariantAString;
    FpPostData: PPSVariantAString;
    FpGetURLBase: PPSVariantAString;
    FpRequestHead: PPSVariantAString;
    FpResponseHead: PPSVariantAString;
    FpScriptBuffer: PPSVariantAString;
    FpVersion: PPSVariantAString;
    FpLang: PPSVariantAString;
    FpGetX: PPSVariantS32;
    FpGetY: PPSVariantS32;
    FpGetZ: PPSVariantS32;
    FpGetLlon: PPSVariantDouble;
    FpGetTLat: PPSVariantDouble;
    FpGetBLat: PPSVariantDouble;
    FpGetRLon: PPSVariantDouble;
    FpGetLMetr: PPSVariantDouble;
    FpGetRMetr: PPSVariantDouble;
    FpGetTMetr: PPSVariantDouble;
    FpGetBMetr: PPSVariantDouble;
    FpConverter: PPSVariantInterface;
    FpDownloader: PPSVariantInterface;
    FpDefProjConverter: PPSVariantInterface;
    FpProjFactory: PPSVariantInterface;
    procedure PrepareCompiledScript(const ACompiledData: TbtString);
    procedure RegisterAppVars;
    procedure RegisterAppRoutines;
    procedure SetVar(
      const ALastResponseInfo: ILastResponseInfo;
      const ADownloaderConfig: ITileDownloaderConfigStatic;
      const ASource: ITileRequest;
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer
    );
    procedure OnLangChange;
  protected
    function BuildRequest(
      const ASource: ITileRequest;
      const ALastResponseInfo: ILastResponseInfo;
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer
    ): ITileDownloadRequest; override;
  public
    constructor Create(
      const ACompiledData: TbtString;
      const AConfig: ITileDownloadRequestBuilderConfig;
      const ATileDownloaderConfig: ITileDownloaderConfig;
      const ADownloader: IDownloader;
      const ACheker: IDownloadChecker;
      const ADefProjConverter: IProjConverter;
      const AProjFactory: IProjConverterFactory;
      const ALangManager: ILanguageManager
    );
    destructor Destroy; override;
  end;

implementation

uses
  t_GeoTypes,
  i_BinaryData,
  i_SimpleHttpDownloader,
  u_BinaryData,
  u_ListenerByEvent,
  u_TileDownloadRequest,
  u_SimpleHttpDownloader,
  u_TileRequestBuilderHelpers,
  u_SimpleFlagWithInterlock,
  u_ResStrings;

{ TTileRequestBuilderPascalScript }

constructor TTileDownloadRequestBuilderPascalScript.Create(
  const ACompiledData: TbtString;
  const AConfig: ITileDownloadRequestBuilderConfig;
  const ATileDownloaderConfig: ITileDownloaderConfig;
  const ADownloader: IDownloader;
  const ACheker: IDownloadChecker;
  const ADefProjConverter: IProjConverter;
  const AProjFactory: IProjConverterFactory;
  const ALangManager: ILanguageManager
);
begin
  inherited Create(AConfig);
  FPSExec := nil;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FLangManager := ALangManager;
  FDownloader := ADownloader;
  FDefProjConverter := ADefProjConverter;
  FProjFactory := AProjFactory;
  FCheker := ACheker;

  FLangChangeFlag := TSimpleFlagWithInterlock.Create;

  FLangListener := TNotifyNoMmgEventListener.Create(Self.OnLangChange);
  FLangManager.GetChangeNotifier.Add(FLangListener);

  FCoordConverter := Config.GeoCoder as ICoordConverterSimple;
  PrepareCompiledScript(ACompiledData);

  OnLangChange;
end;

destructor TTileDownloadRequestBuilderPascalScript.Destroy;
begin
  if Assigned(FLangManager) and Assigned(FLangListener) then begin
    FLangManager.GetChangeNotifier.Remove(FLangListener);
    FLangManager := nil;
    FLangListener := nil;
  end;

  FreeAndNil(FPSExec);
  FCoordConverter := nil;
  FDownloader := nil;

  inherited;
end;

procedure TTileDownloadRequestBuilderPascalScript.OnLangChange;
begin
  FLangChangeFlag.SetFlag;
end;

function TTileDownloadRequestBuilderPascalScript.BuildRequest(
  const ASource: ITileRequest;
  const ALastResponseInfo: ILastResponseInfo;
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer
): ITileDownloadRequest;
var
  VDownloaderConfig: ITileDownloaderConfigStatic;
  VPostData: IBinaryData;
begin
  Result := nil;
  if (ACancelNotifier <> nil) and (not ACancelNotifier.IsOperationCanceled(AOperationID)) then begin
    Lock;
    try
      if (ACancelNotifier <> nil) and (not ACancelNotifier.IsOperationCanceled(AOperationID)) then begin
        VDownloaderConfig := FTileDownloaderConfig.GetStatic;
        SetVar(
          ALastResponseInfo,
          VDownloaderConfig,
          ASource,
          ACancelNotifier,
          AOperationID
        );
        try
          if not FPSExec.RunScript then begin
            FPSExec.RaiseCurrentException;
          end;
        except
          FPSExec.Stop;
          raise;
        end;
        if FpResultUrl.Data <> '' then begin
          if FpPostData.Data <> '' then begin
            VPostData := TBinaryData.CreateByAnsiString(FpPostData.Data);
            Result :=
              TTileDownloadPostRequest.Create(
                FpResultUrl.Data,
                FpRequestHead.Data,
                VPostData,
                VDownloaderConfig.InetConfigStatic,
                FCheker,
                ASource
              );
          end else begin
            Result :=
              TTileDownloadRequest.Create(
                FpResultUrl.Data,
                FpRequestHead.Data,
                VDownloaderConfig.InetConfigStatic,
                FCheker,
                ASource
              );
          end;
        end;
        FScriptBuffer := FpScriptBuffer.Data;
      end;
    finally
      Unlock;
    end;
  end;
end;

procedure TTileDownloadRequestBuilderPascalScript.PrepareCompiledScript(const ACompiledData: TbtString);
begin
  // TODO: compare with TBasePascalScriptCompiled
  FScriptBuffer := '';

  // create
  FPSExec := TBasePascalScriptExec.Create;

  // init by common
  FPSExec.RegisterAppCommonRoutines;

  // init by self
  Self.RegisterAppRoutines;

  // load
  if not FPSExec.LoadData(ACompiledData) then begin
    raise Exception.Create(
      SAS_ERR_UrlScriptByteCodeLoad + #13#10 +
      string(TIFErrorToString(FPSExec.ExceptionCode, FPSExec.ExceptionString))
    );
  end;

  // loaded - add variables
  Self.RegisterAppVars;
end;

procedure TTileDownloadRequestBuilderPascalScript.RegisterAppRoutines;
begin
  // empty
end;

procedure TTileDownloadRequestBuilderPascalScript.RegisterAppVars;
begin
  FpResultUrl := PPSVariantAString(FPSExec.GetVar2('ResultURL'));
  FpPostData := PPSVariantAString(FPSExec.GetVar2('PostData'));
  FpGetURLBase := PPSVariantAString(FPSExec.GetVar2('GetURLBase'));
  FpGetURLBase.Data := '';
  FpRequestHead := PPSVariantAString(FPSExec.GetVar2('RequestHead'));
  FpRequestHead.Data := '';
  FpResponseHead := PPSVariantAString(FPSExec.GetVar2('ResponseHead'));
  FpResponseHead.Data := '';
  FpVersion := PPSVariantAString(FPSExec.GetVar2('Version'));
  FpVersion.Data := '';
  FpLang := PPSVariantAString(FPSExec.GetVar2('Lang'));
  FpLang.Data := '';
  FpScriptBuffer := PPSVariantAString(FPSExec.GetVar2('ScriptBuffer'));
  FpGetX := PPSVariantS32(FPSExec.GetVar2('GetX'));
  FpGetY := PPSVariantS32(FPSExec.GetVar2('GetY'));
  FpGetZ := PPSVariantS32(FPSExec.GetVar2('GetZ'));
  FpGetLlon := PPSVariantDouble(FPSExec.GetVar2('GetLlon'));
  FpGetTLat := PPSVariantDouble(FPSExec.GetVar2('GetTLat'));
  FpGetBLat := PPSVariantDouble(FPSExec.GetVar2('GetBLat'));
  FpGetRLon := PPSVariantDouble(FPSExec.GetVar2('GetRLon'));
  FpGetLMetr := PPSVariantDouble(FPSExec.GetVar2('GetLmetr'));
  FpGetTMetr := PPSVariantDouble(FPSExec.GetVar2('GetTmetr'));
  FpGetBMetr := PPSVariantDouble(FPSExec.GetVar2('GetBmetr'));
  FpGetRMetr := PPSVariantDouble(FPSExec.GetVar2('GetRmetr'));
  FpConverter := PPSVariantInterface(FPSExec.GetVar2('Converter'));
  FpDownloader := PPSVariantInterface(FPSExec.GetVar2('Downloader'));
  FpDefProjConverter := PPSVariantInterface(FPSExec.GetVar2('DefProjConverter'));
  FpProjFactory := PPSVariantInterface(FPSExec.GetVar2('ProjFactory'));
end;

procedure TTileDownloadRequestBuilderPascalScript.SetVar(
  const ALastResponseInfo: ILastResponseInfo;
  const ADownloaderConfig: ITileDownloaderConfigStatic;
  const ASource: ITileRequest;
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer
);
var
  XY: TPoint;
  VLonLat: TDoublePoint;
  VTile: TPoint;
  VZoom: Byte;
  VAccept, VUserAgent: AnsiString;
  VUseDownloader: Boolean;
  VSimpleDownloader: ISimpleHttpDownloader;
begin
  VTile := ASource.Tile;
  VZoom := ASource.Zoom;
  FpGetX.Data := VTile.X;
  FpGetY.Data := VTile.Y;
  FpGetZ.Data := VZoom + 1;
  VLonLat := FCoordConverter.Pos2LonLat(VTile, VZoom);
  FpGetLlon.Data := VLonLat.X;
  FpGetTLat.Data := VLonLat.Y;
  VLonLat := FCoordConverter.LonLat2Metr(VLonLat);
  FpGetLMetr.Data := VLonLat.X;
  FpGetTMetr.Data := VLonLat.Y;
  XY := VTile;
  Inc(XY.X);
  Inc(XY.Y);
  VLonLat := FCoordConverter.Pos2LonLat(XY, VZoom);
  FpGetRLon.Data := VLonLat.X;
  FpGetBLat.Data := VLonLat.Y;
  VLonLat := FCoordConverter.LonLat2Metr(VLonLat);
  FpGetRMetr.Data := VLonLat.X;
  FpGetBMetr.Data := VLonLat.Y;
  FpConverter.Data := FCoordConverter;

  FpResultUrl.Data := '';
  FpPostData.Data := '';
  Config.LockRead;
  try
    FpGetURLBase.Data := Config.UrlBase;
    FpRequestHead.Data := Config.RequestHeader;
    VUseDownloader := Config.IsUseDownloader;
  finally
    Config.UnlockRead;
  end;

  // TODO:  Заменить DefaultMIMEType на отдельную настройку
  if ADownloaderConfig.DefaultMIMEType <> '' then begin
    VAccept := ADownloaderConfig.DefaultMIMEType;
  end else begin
    VAccept := '*/*';
  end;

  FpRequestHead.Data := SetHeaderValue(FpRequestHead.Data, 'Accept', VAccept);
  VUserAgent := GetHeaderValue(FpRequestHead.Data, 'User-Agent');
  if VUserAgent = '' then begin
    FpRequestHead.Data := SetHeaderValue(FpRequestHead.Data, 'User-Agent', ADownloaderConfig.InetConfigStatic.UserAgentString);
  end;

  if ALastResponseInfo <> nil then begin
    FpResponseHead.Data := ALastResponseInfo.ResponseHead;
  end else begin
    FpResponseHead.Data := '';
  end;
  FpScriptBuffer.Data := FScriptBuffer;
  if ASource.VersionInfo <> nil then begin
    FpVersion.Data := ASource.VersionInfo.UrlString;
  end else begin
    FpVersion.Data := '';
  end;
  if FLangChangeFlag.CheckFlagAndReset then begin
    FLang := FLangManager.GetCurrentLanguageCode;
  end;
  FpLang.Data := FLang;
  if FDownloader <> nil then begin
    if VUseDownloader then begin
      VSimpleDownloader :=
        TSimpleHttpDownloader.Create(
          FDownloader,
          ADownloaderConfig.InetConfigStatic,
          ACancelNotifier,
          AOperationID
        );
      FpDownloader.Data := VSimpleDownloader;
    end;
  end else begin
    FpDownloader.Data := nil;
  end;
  FpDefProjConverter.Data := FDefProjConverter;
  FpProjFactory.Data := FProjFactory;
end;

end.
