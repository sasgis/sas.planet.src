{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_InternalDomainUrlHandler;

interface

uses
  i_PathConfig,
  i_InternalDomainUrlHandler,
  i_InternalDomainUrlHandlerConfig,
  u_BaseInterfacedObject;

type
  TInternalDomainUrlHandler = class(TBaseInterfacedObject, IInternalDomainUrlHandler)
  private
    type TCommand = (cmdApp, cmdExplorer, cmdBrowser, cmdUser);
  private
    FUserApps: TUserAppArray;
    FAllowedExt: array of string;
    FMediaDataPath: IPathConfig;
    FMediaDataUrl: string;
    procedure PrepareAllowedExtArray(const AAllowedExt: string);
    function IsAllowedExt(const AExt: string): Boolean;
    function GetCommand(var AUrl: string; out ACmd: TCommand; out ACmdId: string): Boolean;
    procedure InternalUrlToUrl(var AUrl: string);
    function PrepareFileName(const AUrl: string): string;
  private
    { IInternalDomainUrlHandler }
    function Process(const AUrl: string): Boolean;
  public
    constructor Create(
      const AConfig: IInternalDomainUrlHandlerConfig;
      const AMediaDataPath: IPathConfig
    );
  end;

implementation

uses
  SysUtils,
  StrUtils,
  Classes,
  c_InternalBrowser,
  u_InetFunc;

{ TInternalDomainUrlHandler }

constructor TInternalDomainUrlHandler.Create(
  const AConfig: IInternalDomainUrlHandlerConfig;
  const AMediaDataPath: IPathConfig
);
begin
  Assert(AConfig <> nil);
  Assert(AMediaDataPath <> nil);

  inherited Create;
  PrepareAllowedExtArray(AConfig.AllowedExt);
  FUserApps := AConfig.UserAppsConfig.UserApps;
  FMediaDataPath := AMediaDataPath;
  FMediaDataUrl := LowerCase(CMediaDataInternalURL);
end;

procedure TInternalDomainUrlHandler.PrepareAllowedExtArray(const AAllowedExt: string);
var
  I, J: Integer;
  VList: TStringList;
begin
  SetLength(FAllowedExt, 0);
  if AAllowedExt = '' then begin
    Exit;
  end;
  VList := TStringList.Create;
  try
    VList.Delimiter := ';';
    VList.StrictDelimiter := True;
    VList.DelimitedText := LowerCase(AAllowedExt);
    SetLength(FAllowedExt, VList.Count);
    J := 0;
    for I := 0 to VList.Count - 1 do begin
      FAllowedExt[J] := Trim(StringReplace(VList.Strings[I], '.', '', [rfReplaceAll]));
      if FAllowedExt[J] <> '' then begin
        Inc(J);
      end;
    end;
    SetLength(FAllowedExt, J);
  finally
    VList.Free;
  end;
end;

function TInternalDomainUrlHandler.GetCommand(
  var AUrl: string;
  out ACmd: TCommand;
  out ACmdId: string
): Boolean;
var
  I: Integer;
begin
  if EndsStr(CAppCmdPostfix, AUrl) then begin
    ACmd := cmdApp;
    SetLength(AUrl, Length(AUrl) - Length(CAppCmdPostfix));
    Result := True;
  end else
  if EndsStr(CExplorerCmdPostfix, AUrl) then begin
    ACmd := cmdExplorer;
    SetLength(AUrl, Length(AUrl) - Length(CExplorerCmdPostfix));
    Result := True;
  end else
  if EndsStr(CBrowserCmdPostfix, AUrl) then begin
    ACmd := cmdBrowser;
    SetLength(AUrl, Length(AUrl) - Length(CBrowserCmdPostfix));
    Result := True;
  end else begin
    I := Pos(CUserCmdPostfix, AUrl);
    if I > 0 then begin
      ACmd := cmdUser;
      ACmdId := Copy(AUrl, I + Length(CUserCmdPostfix));
      SetLength(AUrl, I-1);
      Result := True;
    end else begin
      Result := False;
    end;
  end;
end;

procedure TInternalDomainUrlHandler.InternalUrlToUrl(var AUrl: string);
begin
  if StartsStr(FMediaDataUrl, AUrl) then begin
    AUrl := IncludeTrailingPathDelimiter(FMediaDataPath.FullPath) + Copy(AUrl, Length(FMediaDataUrl) + 1);
  end else
  if StartsStr(CSASInternalURLPrefix, AUrl) then begin
    AUrl := Copy(AUrl, Length(CSASInternalURLPrefix) + 1);
  end;
end;

function TInternalDomainUrlHandler.IsAllowedExt(const AExt: string): Boolean;
var
  I: Integer;
  VExt: string;
begin
  Result := Length(FAllowedExt) = 0;
  if not Result then begin
    VExt := LowerCase(Trim(AExt));
    if VExt[1] = '.' then begin
      VExt := Copy(AExt, 2);
    end;
    if VExt = '' then begin
      Result := False;
      Exit;
    end;
    for I := 0 to Length(FAllowedExt) - 1 do begin
      if VExt = FAllowedExt[I] then begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TInternalDomainUrlHandler.PrepareFileName(const AUrl: string): string;
var
  I: Integer;
begin
  Result := AUrl;
  InternalUrlToUrl(Result);
  Result := URLDecode(Result);
  I := Length(Result);
  if Result[I] = '/' then begin
    SetLength(Result, I-1);
  end;
end;

function TInternalDomainUrlHandler.Process(const AUrl: string): Boolean;

  procedure _DoOpenFile(const AUrl: string; const AApp: string = '');
  var
    VExt: string;
    VFileName: string;
  begin
    VFileName := PrepareFileName(AUrl);
    if FileExists(VFileName) then begin
      VExt := ExtractFileExt(VFileName);
      if IsAllowedExt(VExt) then begin
        if AApp <> '' then begin
          OpenFileInProgram(VFileName, AApp);
        end else begin
          OpenFileInDefaultProgram(VFileName);
        end;
      end else begin
        raise Exception.CreateFmt(
          'File extention "%s" disabled by config!', [VExt]
        );
      end;
    end else begin
      raise Exception.CreateFmt(
        'Url "%s" process error. File not exists: %s', [AUrl, VFileName]
      );
    end;
  end;

var
  I: Integer;
  VUrl: string;
  VFileName: string;
  VCmd: TCommand;
  VCmdId: string;
begin
  Result := False;

  VUrl := AUrl;
  if not GetCommand(VUrl, VCmd, VCmdId) then begin
    Exit;
  end;

  case VCmd of

    cmdApp: begin
      _DoOpenFile(VUrl);
      Result := True;
    end;

    cmdExplorer: begin
      VFileName := PrepareFileName(VUrl);
      if FileExists(VFileName) then begin
        SelectFileInExplorer(VFileName);
      end else if DirectoryExists(VFileName) then begin
        SelectPathInExplorer(VFileName);
      end else begin
        raise Exception.CreateFmt(
          'Url "%s" process error. File (Dir) not exists: %s', [VUrl, VFileName]
        );
      end;
      Result := True;
    end;

    cmdBrowser: begin
      InternalUrlToUrl(VUrl);
      OpenUrlInBrowser(VUrl);
      Result := True;
    end;

    cmdUser: begin
      for I := 0 to Length(FUserApps) do begin
        if VCmdId = FUserApps[I].ID then begin
          _DoOpenFile(VUrl, FUserApps[I].Path);
          Result := True;
          Break;
        end;
      end;
      if not Result then begin
        raise Exception.Create('Unregistered user app id: ' + VCmdId);
      end;
    end;
  else
    Assert(False);
  end;
end;

end.
