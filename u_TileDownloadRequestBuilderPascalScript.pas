{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  uPSC_dll,
  uPSR_dll,
  uPSRuntime,
  uPSCompiler,
  uPSUtils,
  i_JclNotify,
  i_ConfigDataProvider,
  i_CoordConverter,
  i_ZmpInfo,
  i_TileDownloaderConfig,
  i_TileRequest,
  i_LanguageManager,
  i_CoordConverterFactory,
  i_LastResponseInfo,
  i_TileDownloadRequest,
  i_TileDownloadRequestBuilderConfig,
  u_TileDownloadRequestBuilder;

type
  EPascalScriptRunError = class(Exception);

  TTileDownloadRequestBuilderPascalScript = class(TTileDownloadRequestBuilder)
  private
    FTileDownloaderConfig: ITileDownloaderConfig;
    FCoordConverter: ICoordConverterSimple;
    FScriptBuffer: string;

    FLang: string;
    FLangManager: ILanguageManager;
    FLangListener: IJclListener;
    FLangChangeCount: Integer;

    FExec: TPSExec;
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
    procedure PreparePascalScript(ACompiledData: TbtString);
    procedure SetVar(
      ALastResponseInfo: ILastResponseInfo;
      ADownloaderConfig: ITileDownloaderConfigStatic;
      ASource: ITileRequest
    );
    procedure OnLangChange;
  protected
    function BuildRequest(
      ASource: ITileRequest;
      ALastResponseInfo: ILastResponseInfo
    ): ITileDownloadRequest; override;
  public
    constructor Create(
      ACompiledData: TbtString;
      AConfig: ITileDownloadRequestBuilderConfig;
      ATileDownloaderConfig: ITileDownloaderConfig;
      ALangManager: ILanguageManager
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  Variants,
  u_NotifyEventListener,
  t_GeoTypes,
  u_GeoToStr,
  u_TileDownloadRequest,
  u_TileRequestBuilderHelpers,
  u_ResStrings;

{ TTileRequestBuilderPascalScript }

constructor TTileDownloadRequestBuilderPascalScript.Create(
  ACompiledData: TbtString;
  AConfig: ITileDownloadRequestBuilderConfig;
  ATileDownloaderConfig: ITileDownloaderConfig;
  ALangManager: ILanguageManager
);
begin
  inherited Create(AConfig);
  FTileDownloaderConfig := ATileDownloaderConfig;
  FLangManager := ALangManager;

  FLangListener := TNotifyNoMmgEventListener.Create(Self.OnLangChange);
  FLangManager.GetChangeNotifier.Add(FLangListener);

  FCoordConverter := FConfig.GeoCoder as ICoordConverterSimple;
  PreparePascalScript(ACompiledData);

  OnLangChange;
end;

destructor TTileDownloadRequestBuilderPascalScript.Destroy;
begin
  FLangManager.GetChangeNotifier.Remove(FLangListener);
  FLangManager := nil;
  FLangListener := nil;

  FreeAndNil(FExec);
  FCoordConverter := nil;
  
  inherited;
end;

procedure TTileDownloadRequestBuilderPascalScript.OnLangChange;
begin
  InterlockedIncrement(FLangChangeCount);
end;

function TTileDownloadRequestBuilderPascalScript.BuildRequest(
  ASource: ITileRequest;
  ALastResponseInfo: ILastResponseInfo
): ITileDownloadRequest;
var
  VDownloaderConfig: ITileDownloaderConfigStatic;
begin
  Lock;
  try
    VDownloaderConfig := FTileDownloaderConfig.GetStatic;
    SetVar(
      ALastResponseInfo,
      VDownloaderConfig,
      ASource
    );
    try
      FExec.RunScript;
    except on E: Exception do
      raise EPascalScriptRunError.Create(E.Message);
    end;
    if FpResultUrl.Data <> '' then begin
      Result :=
        TTileDownloadRequest.Create(
          FpResultUrl.Data,
          FpRequestHead.Data,
          VDownloaderConfig.InetConfigStatic,
          ASource
        );
    end;
    FScriptBuffer := FpScriptBuffer.Data;
  finally
    Unlock;
  end;
end;

procedure TTileDownloadRequestBuilderPascalScript.PreparePascalScript(ACompiledData: TbtString);
begin
  FScriptBuffer := '';

  FExec := TPSExec.Create;
  RegisterDLLRuntime(FExec);

  FExec.RegisterDelphiFunction(@RoundEx, 'RoundEx', cdRegister);
  FExec.RegisterDelphiFunction(@IntPower, 'IntPower', cdRegister);
  FExec.RegisterDelphiFunction(@Rand, 'Random', cdRegister);
  FExec.RegisterDelphiFunction(@IntToHex, 'IntToHex', cdRegister);
  FExec.RegisterDelphiFunction(@StrLength, 'Length', cdRegister);
  FExec.RegisterDelphiFunction(@GetAfter, 'GetAfter', cdRegister);
  FExec.RegisterDelphiFunction(@GetBefore, 'GetBefore', cdRegister);
  FExec.RegisterDelphiFunction(@GetBetween, 'GetBetween', cdRegister);
  FExec.RegisterDelphiFunction(@SubStrPos, 'SubStrPos', cdRegister);
  FExec.RegisterDelphiFunction(@RegExprGetMatchSubStr, 'RegExprGetMatchSubStr', cdRegister);
  FExec.RegisterDelphiFunction(@RegExprReplaceMatchSubStr, 'RegExprReplaceMatchSubStr', cdRegister);
  FExec.RegisterDelphiFunction(@SetHeaderValue, 'SetHeaderValue', cdRegister);
  FExec.RegisterDelphiFunction(@GetHeaderValue, 'GetHeaderValue', cdRegister);
  FExec.RegisterDelphiFunction(@DoHttpRequest, 'DoHttpRequest', cdRegister);

  if not FExec.LoadData(ACompiledData) then begin
    raise Exception.Create(
      SAS_ERR_UrlScriptByteCodeLoad + #13#10 +
      TIFErrorToString(FExec.ExceptionCode, FExec.ExceptionString)
    );
  end;

  FpResultUrl := PPSVariantAString(FExec.GetVar2('ResultURL'));
  FpGetURLBase := PPSVariantAString(FExec.GetVar2('GetURLBase'));
  FpGetURLBase.Data := '';
  FpRequestHead := PPSVariantAString(FExec.GetVar2('RequestHead'));
  FpRequestHead.Data := '';
  FpResponseHead := PPSVariantAString(FExec.GetVar2('ResponseHead'));
  FpResponseHead.Data := '';
  FpVersion := PPSVariantAString(FExec.GetVar2('Version'));
  FpVersion.Data := '';
  FpLang := PPSVariantAString(FExec.GetVar2('Lang'));
  FpLang.Data := '';
  FpScriptBuffer := PPSVariantAString(FExec.GetVar2('ScriptBuffer'));
  FpGetX := PPSVariantS32(FExec.GetVar2('GetX'));
  FpGetY := PPSVariantS32(FExec.GetVar2('GetY'));
  FpGetZ := PPSVariantS32(FExec.GetVar2('GetZ'));
  FpGetLlon := PPSVariantDouble(FExec.GetVar2('GetLlon'));
  FpGetTLat := PPSVariantDouble(FExec.GetVar2('GetTLat'));
  FpGetBLat := PPSVariantDouble(FExec.GetVar2('GetBLat'));
  FpGetRLon := PPSVariantDouble(FExec.GetVar2('GetRLon'));
  FpGetLmetr := PPSVariantDouble(FExec.GetVar2('GetLmetr'));
  FpGetTmetr := PPSVariantDouble(FExec.GetVar2('GetTmetr'));
  FpGetBmetr := PPSVariantDouble(FExec.GetVar2('GetBmetr'));
  FpGetRmetr := PPSVariantDouble(FExec.GetVar2('GetRmetr'));
  FpConverter := PPSVariantInterface(FExec.GetVar2('Converter'));
end;

procedure TTileDownloadRequestBuilderPascalScript.SetVar(
  ALastResponseInfo: ILastResponseInfo;
  ADownloaderConfig: ITileDownloaderConfigStatic;
  ASource: ITileRequest
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
    FpVersion.Data := VarToStrDef(ASource.VersionInfo.Version, '');
  end else begin
    FpVersion.Data := '';
  end;
  if InterlockedExchange(FLangChangeCount, 0) > 0 then begin
    FLang := FLangManager.GetCurrentLanguageCode;
  end;
  FpLang.Data := FLang;
end;

end.
