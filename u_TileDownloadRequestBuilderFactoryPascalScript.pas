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
  u_TileDownloaderStateInternal;

type
  EPascalScriptCompileError = class(Exception);

  TTileDownloadRequestBuilderFactoryPascalScript = class(TInterfacedObject, ITileDownloadRequestBuilderFactory)
  private
    FState: ITileDownloaderStateChangeble;
    FStateInternal: ITileDownloaderStateInternal;
    FConfig: ITileDownloadRequestBuilderConfig;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FCheker: IDownloadChecker;
    FLangManager: ILanguageManager;
    FScriptText: string;
    FCompiledData: TbtString;
    FCS: TCriticalSection;
    FScriptInited: Boolean;
    procedure PreparePascalScript(APascalScript: string);
  protected
    function GetState: ITileDownloaderStateChangeble;
    function BuildRequestBuilder(ADownloader: IDownloader): ITileDownloadRequestBuilder;
  public
    constructor Create(
      AScriptText: string;
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
  AScriptText: string;
  AConfig: ITileDownloadRequestBuilderConfig;
  ATileDownloaderConfig: ITileDownloaderConfig;
  ACheker: IDownloadChecker;
  ALangManager: ILanguageManager
);
var
  VState: TTileDownloaderStateInternal;
begin
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

function ScriptOnUses(Sender: TPSPascalCompiler; const Name: string): Boolean;
var
  T: TPSType;
  RecT: TPSRecordType;
begin
  if Name = 'SYSTEM' then begin
    T := Sender.FindType('integer');
    RecT := TPSRecordType(Sender.AddType('TPoint', btRecord));
    with RecT.AddRecVal do begin
      FieldOrgName := 'x';
      aType := t;
    end;

    with RecT.AddRecVal do begin
      FieldOrgName := 'y';
      aType := t;
    end;

    T := Sender.FindType('Double');
    RecT := TPSRecordType(Sender.AddType('TDoublePoint', btRecord));
    with RecT.AddRecVal do begin
      FieldOrgName := 'x';
      aType := t;
    end;

    with RecT.AddRecVal do begin
      FieldOrgName := 'y';
      aType := t;
    end;

    with Sender.AddInterface(Sender.FindInterface('IUnknown'), ICoordConverterSimple, 'ICoordConverter') do begin
      RegisterMethod('function Pos2LonLat(XY : TPoint; Azoom : byte) : TDoublePoint', cdStdCall);
      RegisterMethod('function LonLat2Pos(Ll : TDoublePoint; Azoom : byte) : Tpoint', cdStdCall);
      RegisterMethod('function LonLat2Metr(Ll : TDoublePoint) : TDoublePoint', cdStdCall);
      RegisterMethod('function Metr2LonLat(Mm : TDoublePoint) : TDoublePoint', cdStdCall);

      RegisterMethod('function TilesAtZoom(AZoom: byte): Longint', cdStdCall);
      RegisterMethod('function PixelsAtZoom(AZoom: byte): Longint', cdStdCall);

      RegisterMethod('function TilePos2PixelPos(const XY : TPoint; Azoom : byte): TPoint', cdStdCall);
      RegisterMethod('function TilePos2PixelRect(const XY : TPoint; Azoom : byte): TRect', cdStdCall);
    end;
    T := Sender.FindType('ICoordConverter');
    Sender.AddUsedVariable('Converter', t);

    T := Sender.FindType('string');
    Sender.AddUsedVariable('ResultURL', t);
    Sender.AddUsedVariable('GetURLBase', t);
    Sender.AddUsedVariable('RequestHead', t);
    Sender.AddUsedVariable('ResponseHead', t);
    Sender.AddUsedVariable('ScriptBuffer', t);
    Sender.AddUsedVariable('Version', t);
    Sender.AddUsedVariable('Lang', t);
    T := Sender.FindType('integer');
    Sender.AddUsedVariable('GetX', t);
    Sender.AddUsedVariable('GetY', t);
    Sender.AddUsedVariable('GetZ', t);
    T := Sender.FindType('Double');
    Sender.AddUsedVariable('GetLlon', t);
    Sender.AddUsedVariable('GetTLat', t);
    Sender.AddUsedVariable('GetBLat', t);
    Sender.AddUsedVariable('GetRLon', t);
    Sender.AddUsedVariable('GetLMetr', t);
    Sender.AddUsedVariable('GetRMetr', t);
    Sender.AddUsedVariable('GetTMetr', t);
    Sender.AddUsedVariable('GetBMetr', t);
    Sender.AddDelphiFunction('function Random(x:integer):integer');
    Sender.AddDelphiFunction('function GetUnixTime:int64');
    Sender.AddDelphiFunction('function RoundEx(chislo: Double; Precision: Integer): string');
    Sender.AddDelphiFunction('function IntPower(const Base: Extended; const Exponent: Integer): Extended register');
    Sender.AddDelphiFunction('function IntToHex(Value: Integer; Digits: Integer): string');
    Sender.AddDelphiFunction('function Length(Str: string): integer');
    Sender.AddDelphiFunction('function GetAfter(SubStr, Str: string): string');
    Sender.AddDelphiFunction('function GetBefore(SubStr, Str: string): string');
    Sender.AddDelphiFunction('function GetBetween(Str, After, Before: string): string');
    Sender.AddDelphiFunction('function SubStrPos(const Str, SubStr: String; FromPos: integer): integer');
    Sender.AddDelphiFunction('function RegExprGetMatchSubStr(const Str, MatchExpr: string; AMatchID: Integer): string');
    Sender.AddDelphiFunction('function RegExprReplaceMatchSubStr(const Str, MatchExpr, Replace: string): string');
    Sender.AddDelphiFunction('function SetHeaderValue(AHeaders, AName, AValue: string): string');
    Sender.AddDelphiFunction('function GetHeaderValue(AHeaders, AName: string): string');
    Sender.AddDelphiFunction('function DoHttpRequest(const ARequestUrl, ARequestHeader, APostData: string; out AResponseHeader, AResponseData: string): Cardinal');
    Result := True;
  end else begin
    Result := False;
  end;
end;

procedure TTileDownloadRequestBuilderFactoryPascalScript.PreparePascalScript(
  APascalScript: string);
var
  i: integer;
  VCompilerMsg: string;
  VCompiler: TPSPascalCompiler;
begin
  VCompiler := TPSPascalCompiler.Create;
  try
    VCompiler.OnExternalProc := DllExternalProc;
    VCompiler.OnUses := ScriptOnUses;
    if not VCompiler.Compile(APascalScript) then
    begin
      VCompilerMsg := '';
      for i := 0 to VCompiler.MsgCount - 1 do
        VCompilerMsg := VCompilerMsg + VCompiler.Msg[i].MessageToString + #13#10;
      raise EPascalScriptCompileError.CreateFmt(SAS_ERR_UrlScriptCompileError, [VCompilerMsg]);
    end;
    VCompiler.GetOutput(FCompiledData);
  finally
    VCompiler.Free;
  end;
end;

end.
