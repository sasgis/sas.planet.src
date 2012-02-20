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
  SyncObjs,
  SysUtils,
  uPSUtils,
  i_CoordConverter,
  i_LanguageManager,
  i_Downloader,
  i_DownloadChecker,
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
    FCS: TCriticalSection;
    FScriptInited: Boolean;
  protected
    function DoCompilerOnAuxUses(ACompiler: TBasePascalCompiler; const AName: string): Boolean; override;
  protected
    function GetState: ITileDownloaderStateChangeble;
    function BuildRequestBuilder(ADownloader: IDownloader): ITileDownloadRequestBuilder;
  public
    constructor Create(
      const AScriptText: string;
      AConfig: ITileDownloadRequestBuilderConfig;
      ATileDownloaderConfig: ITileDownloaderConfig;
      ACheker: IDownloadChecker;
      ALangManager: ILanguageManager
    );
    destructor Destroy; override;
  end;

implementation

uses
  uPSC_dll,
  uPSCompiler,
  u_ResStrings,
  u_TileDownloadRequestBuilderPascalScript;

{ TTileDownloadRequestBuilderFactoryPascalScript }

constructor TTileDownloadRequestBuilderFactoryPascalScript.Create(
  const AScriptText: string;
  AConfig: ITileDownloadRequestBuilderConfig;
  ATileDownloaderConfig: ITileDownloaderConfig;
  ACheker: IDownloadChecker;
  ALangManager: ILanguageManager
);
var
  VState: TTileDownloaderStateInternal;
begin
  inherited Create(AScriptText);
  
  FConfig := AConfig;
  FCheker := ACheker;
  FLangManager := ALangManager;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FScriptText := AScriptText;

  FCS := TCriticalSection.Create;
  VState := TTileDownloaderStateInternal.Create;
  FStateInternal := VState;
  FState := VState;

  if FScriptText = '' then begin
    FCompiledData := '';
    FStateInternal.Disable('Empty script');
  end;
end;

destructor TTileDownloadRequestBuilderFactoryPascalScript.Destroy;
begin
  FreeAndNil(FCS);
  inherited;
end;

function TTileDownloadRequestBuilderFactoryPascalScript.DoCompilerOnAuxUses(ACompiler: TBasePascalCompiler;
                                                                            const AName: string): Boolean;
var
  T: TPSType;
begin
  if SameText('SYSTEM', AName) then begin
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
  ADownloader: IDownloader
): ITileDownloadRequestBuilder;
begin
  Result := nil;
  if FStateInternal.Enabled then begin
    try
      if not FScriptInited then begin
        FCS.Acquire;
        try
          if not FScriptInited then begin
            try
              PreparePascalScript(FScriptText);
              FScriptInited := True;
            except
              on E:EPascalScriptCompileError do begin
                FStateInternal.Disable(E.Message);
                FCompiledData := '';
              end;
              on E: Exception do begin
                FStateInternal.Disable('Unknown script compile error: ' + E.Message);
                FCompiledData := '';
              end;
            end;
          end;
        finally
          FCS.Release;
        end;
      end;
      Result :=
        TTileDownloadRequestBuilderPascalScript.Create(
          FCompiledData,
          FConfig,
          FTileDownloaderConfig,
          ADownloader,
          FCheker,
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
