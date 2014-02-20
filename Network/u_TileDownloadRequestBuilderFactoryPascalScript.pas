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

unit u_TileDownloadRequestBuilderFactoryPascalScript;

interface

uses
  SysUtils,
  uPSUtils,
  i_LanguageManager,
  i_Downloader,
  i_DownloadChecker,
  i_ProjConverter,
  i_TileDownloaderConfig,
  i_TileDownloaderState,
  i_TileDownloadRequestBuilderConfig,
  i_TileDownloadRequestBuilder,
  i_TileDownloadRequestBuilderFactory,
  u_BasePascalCompiler,
  u_TileDownloaderStateInternal;

type
  TTileDownloadRequestBuilderFactoryPascalScript = class(TBaseFactoryPascalScript, ITileDownloadRequestBuilderFactory)
  private
    FState: ITileDownloaderStateChangeble;
    FStateInternal: ITileDownloaderStateInternal;
    FConfig: ITileDownloadRequestBuilderConfig;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FCheker: IDownloadChecker;
    FLangManager: ILanguageManager;
    FCS: IReadWriteSync;
    FScriptInited: Boolean;
    FDefProjConverter: IProjConverter;
    FProjFactory: IProjConverterFactory;
  protected
    function DoCompilerOnAuxUses(
      ACompiler: TBasePascalCompiler;
      const AName: tbtString
    ): Boolean; override;
  protected
    function GetState: ITileDownloaderStateChangeble;
    function BuildRequestBuilder(const ADownloader: IDownloader): ITileDownloadRequestBuilder;
  public
    constructor Create(
      const AScriptText: AnsiString;
      const AConfig: ITileDownloadRequestBuilderConfig;
      const ATileDownloaderConfig: ITileDownloaderConfig;
      const ACheker: IDownloadChecker;
      const AProjFactory: IProjConverterFactory;
      const ALangManager: ILanguageManager
    );
  end;

implementation

uses
  ALString,
  uPSCompiler,
  u_Synchronizer,
  u_TileDownloadRequestBuilderPascalScript;

{ TTileDownloadRequestBuilderFactoryPascalScript }

constructor TTileDownloadRequestBuilderFactoryPascalScript.Create(
  const AScriptText: AnsiString;
  const AConfig: ITileDownloadRequestBuilderConfig;
  const ATileDownloaderConfig: ITileDownloaderConfig;
  const ACheker: IDownloadChecker;
  const AProjFactory: IProjConverterFactory;
  const ALangManager: ILanguageManager
);
var
  VState: TTileDownloaderStateInternal;
begin
  inherited Create(AScriptText);

  FConfig := AConfig;
  FCheker := ACheker;
  FLangManager := ALangManager;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FProjFactory := AProjFactory;

  FCS := MakeSyncRW_Std(Self, False);
  VState := TTileDownloaderStateInternal.Create;
  FStateInternal := VState;
  FState := VState;

  if AScriptText = '' then begin
    FStateInternal.Disable('Empty script');
  end;
end;

function TTileDownloadRequestBuilderFactoryPascalScript.DoCompilerOnAuxUses(
  ACompiler: TBasePascalCompiler;
  const AName: tbtString
): Boolean;
var
  VType: TPSType;
begin
  if ALSameText('SYSTEM', AName) then begin
    VType := ACompiler.FindType('ISimpleHttpDownloader');
    ACompiler.AddUsedVariable('Downloader', VType);

    VType := ACompiler.FindType('IProjConverter');
    ACompiler.AddUsedVariable('DefProjConverter', VType);

    VType := ACompiler.FindType('IProjConverterFactory');
    ACompiler.AddUsedVariable('ProjFactory', VType);

    VType := ACompiler.FindType('ICoordConverter');
    ACompiler.AddUsedVariable('Converter', VType);

    VType := ACompiler.FindType('AnsiString');
    ACompiler.AddUsedVariable('ResultURL', VType);
    ACompiler.AddUsedVariable('PostData', VType);
    ACompiler.AddUsedVariable('GetURLBase', VType);
    ACompiler.AddUsedVariable('RequestHead', VType);
    ACompiler.AddUsedVariable('ResponseHead', VType);
    ACompiler.AddUsedVariable('ScriptBuffer', VType);
    ACompiler.AddUsedVariable('Version', VType);
    ACompiler.AddUsedVariable('Lang', VType);

    VType := ACompiler.FindType('integer');
    ACompiler.AddUsedVariable('GetX', VType);
    ACompiler.AddUsedVariable('GetY', VType);
    ACompiler.AddUsedVariable('GetZ', VType);

    VType := ACompiler.FindType('Double');
    ACompiler.AddUsedVariable('GetLlon', VType);
    ACompiler.AddUsedVariable('GetTLat', VType);
    ACompiler.AddUsedVariable('GetBLat', VType);
    ACompiler.AddUsedVariable('GetRLon', VType);
    ACompiler.AddUsedVariable('GetLMetr', VType);
    ACompiler.AddUsedVariable('GetRMetr', VType);
    ACompiler.AddUsedVariable('GetTMetr', VType);
    ACompiler.AddUsedVariable('GetBMetr', VType);

    Result := True;
  end else begin
    Result := False;
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
              PreparePascalScript;
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
          CompiledData,
          FConfig,
          FTileDownloaderConfig,
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
