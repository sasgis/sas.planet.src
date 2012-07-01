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
      const AName: string
    ): Boolean; override;
  protected
    function GetState: ITileDownloaderStateChangeble;
    function BuildRequestBuilder(const ADownloader: IDownloader): ITileDownloadRequestBuilder;
  public
    constructor Create(
      const AScriptText: string;
      const AConfig: ITileDownloadRequestBuilderConfig;
      const ATileDownloaderConfig: ITileDownloaderConfig;
      const ACheker: IDownloadChecker;
      const AProjFactory: IProjConverterFactory;
      const ALangManager: ILanguageManager
    );
    destructor Destroy; override;
  end;

implementation

uses
  uPSCompiler,
  u_Synchronizer,
  u_TileDownloadRequestBuilderPascalScript;

{ TTileDownloadRequestBuilderFactoryPascalScript }

constructor TTileDownloadRequestBuilderFactoryPascalScript.Create(
  const AScriptText: string;
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

destructor TTileDownloadRequestBuilderFactoryPascalScript.Destroy;
begin
  FCS := nil;
  inherited;
end;

function TTileDownloadRequestBuilderFactoryPascalScript.DoCompilerOnAuxUses(
  ACompiler: TBasePascalCompiler;
  const AName: string
): Boolean;
var
  T: TPSType;
begin
  if SameText('SYSTEM', AName) then begin
    T := ACompiler.FindType('ISimpleHttpDownloader');
    ACompiler.AddUsedVariable('Downloader', t);

    T := ACompiler.FindType('IProjConverter');
    ACompiler.AddUsedVariable('DefProjConverter', t);

    T := ACompiler.FindType('IProjConverterFactory');
    ACompiler.AddUsedVariable('ProjFactory', t);

    T := ACompiler.FindType('ICoordConverter');
    ACompiler.AddUsedVariable('Converter', t);

    T := ACompiler.FindType('string');
    ACompiler.AddUsedVariable('ResultURL', t);
    ACompiler.AddUsedVariable('GetURLBase', t);
    ACompiler.AddUsedVariable('RequestHead', t);
    ACompiler.AddUsedVariable('ResponseHead', t);
    ACompiler.AddUsedVariable('ScriptBuffer', t);
    ACompiler.AddUsedVariable('Version', t);
    ACompiler.AddUsedVariable('Lang', t);

    T := ACompiler.FindType('integer');
    ACompiler.AddUsedVariable('GetX', t);
    ACompiler.AddUsedVariable('GetY', t);
    ACompiler.AddUsedVariable('GetZ', t);

    T := ACompiler.FindType('Double');
    ACompiler.AddUsedVariable('GetLlon', t);
    ACompiler.AddUsedVariable('GetTLat', t);
    ACompiler.AddUsedVariable('GetBLat', t);
    ACompiler.AddUsedVariable('GetRLon', t);
    ACompiler.AddUsedVariable('GetLMetr', t);
    ACompiler.AddUsedVariable('GetRMetr', t);
    ACompiler.AddUsedVariable('GetTMetr', t);
    ACompiler.AddUsedVariable('GetBMetr', t);

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
  VProjArgs: string;
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
