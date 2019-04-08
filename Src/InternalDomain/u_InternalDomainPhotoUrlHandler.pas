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

unit u_InternalDomainPhotoUrlHandler;

interface

uses
  i_PathConfig,
  i_InternalDomainUrlHandler,
  u_BaseInterfacedObject;

type
  TInternalDomainPhotoUrlHandler = class(TBaseInterfacedObject, IInternalDomainUrlHandler)
  private
    type TCommand = (cmdApp, cmdExplorer, cmdBrowser, cmdUser);
  private
    FMediaDataPath: IPathConfig;
    FMediaDataUrl: string;
    function GetCommand(var AUrl: string; out ACmd: TCommand; out ACmdId: string): Boolean;
    procedure InternalUrlToUrl(var AUrl: string);
    function PrepareFileName(const AUrl: string): string;
  private
    function Process(const AUrl: string): Boolean;
  public
    constructor Create(const AMediaDataPath: IPathConfig);
  end;

implementation

uses
  SysUtils,
  StrUtils,
  c_InternalBrowser,
  u_InetFunc;

{ TInternalDomainPhotoUrlHandler }

constructor TInternalDomainPhotoUrlHandler.Create(
  const AMediaDataPath: IPathConfig
);
begin
  Assert(AMediaDataPath <> nil);

  inherited Create;
  FMediaDataPath := AMediaDataPath;
  FMediaDataUrl := LowerCase(CMediaDataInternalURL);
end;

function TInternalDomainPhotoUrlHandler.GetCommand(
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

procedure TInternalDomainPhotoUrlHandler.InternalUrlToUrl(var AUrl: string);
begin
  if StartsStr(FMediaDataUrl, AUrl) then begin
    AUrl := FMediaDataPath.FullPath + Copy(AUrl, Length(FMediaDataUrl) + 1);
  end else
  if StartsStr(CSASInternalURLPrefix, AUrl) then begin
    AUrl := Copy(AUrl, Length(CSASInternalURLPrefix) + 1);
  end;
end;

function TInternalDomainPhotoUrlHandler.PrepareFileName(const AUrl: string): string;
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

function TInternalDomainPhotoUrlHandler.Process(const AUrl: string): Boolean;
var
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
      VFileName := PrepareFileName(VUrl);
      if FileExists(VFileName) then begin
        OpenFileInDefaultProgram(VFileName);
      end else begin
        raise Exception.CreateFmt(
          'Url "%s" process error. File not exists: %s', [VUrl, VFileName]
        );
      end;
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
      // ToDo
      Result := True;
    end;
  else
    Assert(False);
  end;
end;

end.
