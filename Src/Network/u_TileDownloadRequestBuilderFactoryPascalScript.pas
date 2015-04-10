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

unit u_TileDownloadRequestBuilderFactoryPascalScript;

interface

uses
  SysUtils,
  i_LanguageManager,
  i_Downloader,
  i_CoordConverter,
  i_DownloadChecker,
  i_ProjConverter,
  i_TileDownloaderConfig,
  i_TileDownloaderState,
  i_TileDownloadRequestBuilderConfig,
  i_TileDownloadRequestBuilder,
  i_TileDownloadRequestBuilderFactory,
  u_BaseInterfacedObject,
  u_TileDownloaderStateInternal;

type
  TTileDownloadRequestBuilderFactoryPascalScript = class(TBaseInterfacedObject, ITileDownloadRequestBuilderFactory)
  private
    FState: ITileDownloaderStateChangeble;
    FStateInternal: ITileDownloaderStateInternal;
    FConfig: ITileDownloadRequestBuilderConfig;
    FCoordConverter: ICoordConverter;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FCheker: IDownloadChecker;
    FLangManager: ILanguageManager;
    FCS: IReadWriteSync;
    FCompiledData: AnsiString;
    FScriptText: AnsiString;
    FScriptInited: Boolean;
    FDefProjConverter: IProjConverter;
    FProjFactory: IProjConverterFactory;
    procedure DoCompileScript;
  protected
    function GetState: ITileDownloaderStateChangeble;
    function BuildRequestBuilder(const ADownloader: IDownloader): ITileDownloadRequestBuilder;
  public
    constructor Create(
      const AScriptText: AnsiString;
      const AConfig: ITileDownloadRequestBuilderConfig;
      const ATileDownloaderConfig: ITileDownloaderConfig;
      const ACoordConverter: ICoordConverter;
      const ACheker: IDownloadChecker;
      const AProjFactory: IProjConverterFactory;
      const ALangManager: ILanguageManager
    );
  end;

implementation

uses
  t_PascalScript,
  u_Synchronizer,
  u_PascalScriptTypes,
  u_PascalScriptCompiler,
  u_TileDownloadRequestBuilderPascalScript,
  u_TileDownloadRequestBuilderPascalScriptVars;

{ TTileDownloadRequestBuilderFactoryPascalScript }

constructor TTileDownloadRequestBuilderFactoryPascalScript.Create(
  const AScriptText: AnsiString;
  const AConfig: ITileDownloadRequestBuilderConfig;
  const ATileDownloaderConfig: ITileDownloaderConfig;
  const ACoordConverter: ICoordConverter;
  const ACheker: IDownloadChecker;
  const AProjFactory: IProjConverterFactory;
  const ALangManager: ILanguageManager
);
var
  VState: TTileDownloaderStateInternal;
begin
  inherited Create;
  FScriptText := AScriptText;
  FConfig := AConfig;
  FCheker := ACheker;
  FLangManager := ALangManager;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FCoordConverter := ACoordConverter;
  FProjFactory := AProjFactory;

  FCS := GSync.SyncStd.Make(Self.ClassName);
  VState := TTileDownloaderStateInternal.Create;
  FStateInternal := VState;
  FState := VState;

  if FScriptText = '' then begin
    FStateInternal.Disable('Empty script');
  end;

  FCompiledData := '';
end;

procedure TTileDownloadRequestBuilderFactoryPascalScript.DoCompileScript;

  function _GetRegProcArray: TOnCompileTimeRegProcArray;
  begin
    SetLength(Result, 5);
    Result[0] := @CompileTimeReg_ProjConverter;
    Result[1] := @CompileTimeReg_ProjConverterFactory;
    Result[2] := @CompileTimeReg_CoordConverterSimple;
    Result[3] := @CompileTimeReg_SimpleHttpDownloader;
    Result[4] := @CompileTimeReg_RequestBuilderVars;
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
      if not FScriptInited then begin
        FCS.BeginWrite;
        try
          if not FScriptInited then begin
            try
              VProjArgs := FConfig.DefaultProjConverterArgs;
              if VProjArgs <> '' then begin
                FDefProjConverter := FProjFactory.GetByInitString(VProjArgs);
              end;
              DoCompileScript;
              FScriptInited := True;
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
          FCS.EndWrite;
        end;
      end;
      Result :=
        TTileDownloadRequestBuilderPascalScript.Create(
          FCompiledData,
          FConfig,
          FTileDownloaderConfig,
          FCoordConverter,
          ADownloader,
          FCheker,
          FDefProjConverter,
          FProjFactory,
          FLangManager
        );
    except
      on E: Exception do begin
        FStateInternal.Disable('Request builder create error: ' + E.Message);
      end;
    end;
  end;
end;

end.
