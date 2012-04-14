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
  i_JclNotify,
  i_CoordConverter,
  i_OperationNotifier,
  i_TileDownloaderConfig,
  i_TileRequest,
  i_Downloader,
  i_DownloadChecker,
  i_LanguageManager,
  i_LastResponseInfo,
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
    FScriptBuffer: string;

    FLang: string;
    FLangManager: ILanguageManager;
    FLangListener: IJclListener;
    FLangChangeCount: Integer;

    FPSExec: TBasePascalScriptExec;
    FpResultUrl: PPSVariantAString;
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
    procedure PrepareCompiledScript(const ACompiledData: TbtString);
    procedure RegisterAppVars;
    procedure RegisterAppRoutines;
    procedure SetVar(
      const ALastResponseInfo: ILastResponseInfo;
      const ADownloaderConfig: ITileDownloaderConfigStatic;
      const ASource: ITileRequest
    );
    procedure OnLangChange;
  protected
    function BuildRequest(
      const ASource: ITileRequest;
      const ALastResponseInfo: ILastResponseInfo;
      const ACancelNotifier: IOperationNotifier;
      AOperationID: Integer
    ): ITileDownloadRequest; override;
  public
    constructor Create(
      const ACompiledData: TbtString;
      const AConfig: ITileDownloadRequestBuilderConfig;
      const ATileDownloaderConfig: ITileDownloaderConfig;
      const ADownloader: IDownloader;
      const ACheker: IDownloadChecker;
      const ALangManager: ILanguageManager
    );
    destructor Destroy; override;
  end;

implementation

uses
  t_GeoTypes,
  u_NotifyEventListener,
  u_TileDownloadRequest,
  u_TileRequestBuilderHelpers,
  u_ResStrings;

{ TTileRequestBuilderPascalScript }

constructor TTileDownloadRequestBuilderPascalScript.Create(
  const ACompiledData: TbtString;
  const AConfig: ITileDownloadRequestBuilderConfig;
  const ATileDownloaderConfig: ITileDownloaderConfig;
  const ADownloader: IDownloader;
  const ACheker: IDownloadChecker;
  const ALangManager: ILanguageManager
);
begin
  inherited Create(AConfig);
  FPSExec := nil;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FLangManager := ALangManager;
  FDownloader := ADownloader;
  FCheker := ACheker;

  FLangListener := TNotifyNoMmgEventListener.Create(Self.OnLangChange);
  FLangManager.GetChangeNotifier.Add(FLangListener);

  FCoordConverter := FConfig.GeoCoder as ICoordConverterSimple;
  PrepareCompiledScript(ACompiledData);

  OnLangChange;
end;

destructor TTileDownloadRequestBuilderPascalScript.Destroy;
begin
  FLangManager.GetChangeNotifier.Remove(FLangListener);
  FLangManager := nil;
  FLangListener := nil;

  FreeAndNil(FPSExec);
  FCoordConverter := nil;
  FDownloader := nil;

  inherited;
end;

procedure TTileDownloadRequestBuilderPascalScript.OnLangChange;
begin
  InterlockedIncrement(FLangChangeCount);
end;

function TTileDownloadRequestBuilderPascalScript.BuildRequest(
  const ASource: ITileRequest;
  const ALastResponseInfo: ILastResponseInfo;
  const ACancelNotifier: IOperationNotifier;
  AOperationID: Integer
): ITileDownloadRequest;
var
  VDownloaderConfig: ITileDownloaderConfigStatic;
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
          ASource
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
          Result :=
            TTileDownloadRequest.Create(
              FpResultUrl.Data,
              FpRequestHead.Data,
              VDownloaderConfig.InetConfigStatic,
              FCheker,
              ASource
            );
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
      TIFErrorToString(FPSExec.ExceptionCode, FPSExec.ExceptionString)
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
  FpGetLmetr := PPSVariantDouble(FPSExec.GetVar2('GetLmetr'));
  FpGetTmetr := PPSVariantDouble(FPSExec.GetVar2('GetTmetr'));
  FpGetBmetr := PPSVariantDouble(FPSExec.GetVar2('GetBmetr'));
  FpGetRmetr := PPSVariantDouble(FPSExec.GetVar2('GetRmetr'));
  FpConverter := PPSVariantInterface(FPSExec.GetVar2('Converter'));
end;

procedure TTileDownloadRequestBuilderPascalScript.SetVar(
  const ALastResponseInfo: ILastResponseInfo;
  const ADownloaderConfig: ITileDownloaderConfigStatic;
  const ASource: ITileRequest
);
var
  XY: TPoint;
  Ll: TDoublePoint;
  VTile: TPoint;
  VZoom: Byte;
  VAccept: string;
begin
  VTile := ASource.Tile;
  VZoom := ASource.Zoom;
  FpGetX.Data := VTile.X;
  FpGetY.Data := VTile.Y;
  FpGetZ.Data := VZoom + 1;
  Ll := FCoordConverter.Pos2LonLat(VTile, VZoom);
  FpGetLlon.Data := Ll.X;
  FpGetTLat.Data := Ll.Y;
  Ll := FCoordConverter.LonLat2Metr(LL);
  FpGetLMetr.Data := Ll.X;
  FpGetTMetr.Data := Ll.Y;
  XY := VTile;
  Inc(XY.X);
  Inc(XY.Y);
  Ll := FCoordConverter.Pos2LonLat(XY, VZoom);
  FpGetRLon.Data := Ll.X;
  FpGetBLat.Data := Ll.Y;
  Ll := FCoordConverter.LonLat2Metr(LL);
  FpGetRMetr.Data := Ll.X;
  FpGetBMetr.Data := Ll.Y;
  FpConverter.Data := FCoordConverter;
  FpResultUrl.Data := '';
  FConfig.LockRead;
  try
    FpGetURLBase.Data := FConfig.URLBase;
    FpRequestHead.Data := FConfig.RequestHeader;

  finally
    FConfig.UnlockRead;
  end;

  // TODO:  Заменить DefaultMIMEType на отдельную настройку
  if ADownloaderConfig.DefaultMIMEType <> '' then begin
    VAccept := ADownloaderConfig.DefaultMIMEType
  end else begin
    VAccept := '*/*';
  end;

  SetHeaderValue(FpRequestHead.Data, 'Accept', VAccept);
  SetHeaderValue(FpRequestHead.Data, 'User-Agent', ADownloaderConfig.InetConfigStatic.UserAgentString);

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
  if InterlockedExchange(FLangChangeCount, 0) > 0 then begin
    FLang := FLangManager.GetCurrentLanguageCode;
  end;
  FpLang.Data := FLang;
end;

end.
