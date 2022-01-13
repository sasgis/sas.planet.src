{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_TileDownloadRequestBuilderFactoryPascalScript;

interface

uses
  SysUtils,
  i_LanguageManager,
  i_Downloader,
  i_CoordConverterSimple,
  i_ProjectionSet,
  i_DownloadChecker,
  i_ProjConverter,
  i_TileStorage,
  i_MapVersionFactory,
  i_ContentTypeManager,
  i_TileDownloaderConfig,
  i_TileDownloaderState,
  i_TileDownloadRequestBuilderConfig,
  i_TileDownloadRequestBuilder,
  i_TileDownloadRequestBuilderFactory,
  i_PascalScriptGlobal,
  i_PascalScriptLogger,
  i_PascalScriptTileCache,
  u_BaseInterfacedObject,
  u_TileDownloaderStateInternal;

type
  TTileDownloadRequestBuilderFactoryPascalScript = class(TBaseInterfacedObject, ITileDownloadRequestBuilderFactory)
  private
    FState: ITileDownloaderStateChangeble;
    FStateInternal: ITileDownloaderStateInternal;
    FConfig: ITileDownloadRequestBuilderConfig;
    FProjectionSet: IProjectionSet;
    FCoordConverter: ICoordConverterSimple;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FCheker: IDownloadChecker;
    FLangManager: ILanguageManager;
    FLock: IReadWriteSync;
    FCompiledData: AnsiString;
    FScriptText: AnsiString;
    FIsScriptInitialized: Boolean;
    FDefProjConverter: IProjConverter;
    FProjFactory: IProjConverterFactory;
    FPSGlobal: IPascalScriptGlobal;
    FPSLogger: IPascalScriptLogger;
    FPSTileCache: IPascalScriptTileCache;
    procedure DoCompileScript;
  protected
    function GetState: ITileDownloaderStateChangeble;
    function BuildRequestBuilder(const ADownloader: IDownloader): ITileDownloadRequestBuilder;
  public
    constructor Create(
      const AZmpFileName: string;
      const AScriptText: AnsiString;
      const AConfig: ITileDownloadRequestBuilderConfig;
      const ATileDownloaderConfig: ITileDownloaderConfig;
      const AProjectionSet: IProjectionSet;
      const ACheker: IDownloadChecker;
      const AProjFactory: IProjConverterFactory;
      const ALangManager: ILanguageManager;
      const AStorage: ITileStorage;
      const AMapVersionFactory: IMapVersionFactory;
      const AContentTypeManager: IContentTypeManager
    );
  end;

implementation

uses
  t_PascalScript,
  u_GlobalState,
  u_Synchronizer,
  u_CoordConverterSimpleByProjectionSet,
  u_PascalScriptTypes,
  u_PascalScriptGlobal,
  u_PascalScriptLogger,
  u_PascalScriptTileCache,
  u_PascalScriptWriteLn,
  u_PascalScriptUrlTemplate,
  u_PascalScriptCompiler,
  u_TileDownloadRequestBuilderPascalScript,
  u_TileDownloadRequestBuilderPascalScriptVars;

{ TTileDownloadRequestBuilderFactoryPascalScript }

constructor TTileDownloadRequestBuilderFactoryPascalScript.Create(
  const AZmpFileName: string;
  const AScriptText: AnsiString;
  const AConfig: ITileDownloadRequestBuilderConfig;
  const ATileDownloaderConfig: ITileDownloaderConfig;
  const AProjectionSet: IProjectionSet;
  const ACheker: IDownloadChecker;
  const AProjFactory: IProjConverterFactory;
  const ALangManager: ILanguageManager;
  const AStorage: ITileStorage;
  const AMapVersionFactory: IMapVersionFactory;
  const AContentTypeManager: IContentTypeManager
);
var
  VState: TTileDownloaderStateInternal;
begin
  inherited Create;

  FScriptText := AScriptText;
  FConfig := AConfig;
  FCheker := ACheker;
  FLangManager := ALangManager;
  FProjectionSet := AProjectionSet;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FProjFactory := AProjFactory;

  VState := TTileDownloaderStateInternal.Create;
  FStateInternal := VState;
  FState := VState;

  if FScriptText = '' then begin
    // In case when script is empty we will use
    // TPascalScriptUrlTemplate.Render() to get url from template
    // http://www.sasgis.org/mantis/view.php?id=3610
    FIsScriptInitialized := True;
  end else begin
    FPSGlobal := TPascalScriptGlobal.Create;

    FPSLogger := TPascalScriptLogger.Create(
      GState.AppEnum.CurrentID,
      GState.Config.LogsPath.FullPath,
      AZmpFileName
    );

    FPSTileCache := TPascalScriptTileCache.Create(
      AStorage,
      AMapVersionFactory,
      AContentTypeManager
    );

    FCoordConverter := TCoordConverterSimpleByProjectionSet.Create(AProjectionSet);

    FLock := GSync.SyncStd.Make(Self.ClassName);
  end;

  FCompiledData := '';
end;

procedure TTileDownloadRequestBuilderFactoryPascalScript.DoCompileScript;

  function _GetRegProcArray: TOnCompileTimeRegProcArray;
  begin
    SetLength(Result, 10);
    Result[0] := @CompileTimeReg_ProjConverter;
    Result[1] := @CompileTimeReg_ProjConverterFactory;
    Result[2] := @CompileTimeReg_CoordConverterSimple;
    Result[3] := @CompileTimeReg_SimpleHttpDownloader;
    Result[4] := @CompileTimeReg_PascalScriptGlobal;
    Result[5] := @CompileTimeReg_WriteLn;
    Result[6] := @CompileTimeReg_UrlTemplate;
    Result[7] := @CompileTimeReg_PascalScriptLogger;
    Result[8] := @CompileTimeReg_PascalScriptTileCache;
    Result[9] := @CompileTimeReg_RequestBuilderVars; // must always be the last
  end;

var
  VCompiler: TPascalScriptCompiler;
begin
  VCompiler := TPascalScriptCompiler.Create(FScriptText, _GetRegProcArray);
  try
    if not VCompiler.CompileAndGetOutput(FCompiledData) then begin
      FCompiledData := '';
    end;
  finally
    VCompiler.Free;
  end;
end;

function TTileDownloadRequestBuilderFactoryPascalScript.GetState: ITileDownloaderStateChangeble;
begin
  Result := FState;
end;

function TTileDownloadRequestBuilderFactoryPascalScript.BuildRequestBuilder(
  const ADownloader: IDownloader
): ITileDownloadRequestBuilder;
var
  VProjArgs: AnsiString;
begin
  Result := nil;
  if FStateInternal.Enabled then begin
    try
      if not FIsScriptInitialized then begin
        FLock.BeginWrite;
        try
          if not FIsScriptInitialized then begin
            try
              VProjArgs := FConfig.DefaultProjConverterArgs;
              if VProjArgs <> '' then begin
                FDefProjConverter := FProjFactory.GetByInitString(VProjArgs);
              end;
              DoCompileScript;
              FIsScriptInitialized := True;
            except
              on E: EPascalScriptCompileError do begin
                FStateInternal.Disable(E.Message);
              end;
              on E: Exception do begin
                FStateInternal.Disable('Unknown script compile error: ' + E.Message);
              end;
            end;
          end;
        finally
          FLock.EndWrite;
        end;
      end;
      Result :=
        TTileDownloadRequestBuilderPascalScript.Create(
          FCompiledData,
          FConfig,
          FTileDownloaderConfig,
          FProjectionSet,
          FCoordConverter,
          ADownloader,
          FCheker,
          FDefProjConverter,
          FProjFactory,
          FLangManager,
          FPSGlobal,
          FPSLogger,
          FPSTileCache
        );
    except
      on E: Exception do begin
        FStateInternal.Disable('Request builder create error: ' + E.Message);
      end;
    end;
  end;
end;

end.
