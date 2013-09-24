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

unit u_TileDownloaderConfigStatic;

interface

uses
  Types,
  i_InetConfig,
  i_TileDownloaderConfig,
  u_BaseInterfacedObject;

type
  TTileDownloaderConfigStatic = class(TBaseInterfacedObject, ITileDownloaderConfigStatic)
  private
    FInetConfigStatic: IInetConfigStatic;
    FEnabled: Boolean;
    FAllowUseCookie: Boolean;
    FWaitInterval: Cardinal;
    FMaxConnectToServerCount: Cardinal;
    FIgnoreMIMEType: Boolean;
    FExpectedMIMETypes: AnsiString;
    FDefaultMIMEType: AnsiString;
    FIteratorSubRectSize: TPoint;
    FRestartDownloadOnMemCacheTTL: Boolean;
  private
    function GetInetConfigStatic: IInetConfigStatic;
    function GetEnabled: Boolean;
    function GetAllowUseCookie: Boolean;
    function GetWaitInterval: Cardinal;
    function GetMaxConnectToServerCount: Cardinal;
    function GetIgnoreMIMEType: Boolean;
    function GetExpectedMIMETypes: AnsiString;
    function GetDefaultMIMEType: AnsiString;
    function GetIteratorSubRectSize: TPoint;
    function GetRestartDownloadOnMemCacheTTL: Boolean;
  public
    constructor Create(
      const AInetConfigStatic: IInetConfigStatic;
      AEnabled: Boolean;
      const AAllowUseCookie: Boolean;
      AWaitInterval: Cardinal;
      AMaxConnectToServerCount: Cardinal;
      AIgnoreMIMEType: Boolean;
      const AExpectedMIMETypes: AnsiString;
      const ADefaultMIMEType: AnsiString;
      const AIteratorSubRectSize: TPoint;
      const ARestartDownloadOnMemCacheTTL: Boolean
    );
  end;

implementation

{ TTileDownloaderConfigStatic }

constructor TTileDownloaderConfigStatic.Create(
  const AInetConfigStatic: IInetConfigStatic;
  AEnabled: Boolean;
  const AAllowUseCookie: Boolean;
  AWaitInterval: Cardinal;
  AMaxConnectToServerCount: Cardinal;
  AIgnoreMIMEType: Boolean;
  const AExpectedMIMETypes, ADefaultMIMEType: AnsiString;
  const AIteratorSubRectSize: TPoint;
  const ARestartDownloadOnMemCacheTTL: Boolean
);
begin
  inherited Create;
  FInetConfigStatic := AInetConfigStatic;
  FEnabled := AEnabled;
  FAllowUseCookie := AAllowUseCookie;
  FWaitInterval := AWaitInterval;
  FMaxConnectToServerCount := AMaxConnectToServerCount;
  FIgnoreMIMEType := AIgnoreMIMEType;
  FExpectedMIMETypes := AExpectedMIMETypes;
  FDefaultMIMEType := ADefaultMIMEType;
  FIteratorSubRectSize := AIteratorSubRectSize;
  FRestartDownloadOnMemCacheTTL := ARestartDownloadOnMemCacheTTL;
end;

function TTileDownloaderConfigStatic.GetIteratorSubRectSize: TPoint;
begin
  Result := FIteratorSubRectSize;
end;

function TTileDownloaderConfigStatic.GetAllowUseCookie: Boolean;
begin
  Result := FAllowUseCookie;
end;

function TTileDownloaderConfigStatic.GetDefaultMIMEType: AnsiString;
begin
  Result := FDefaultMIMEType;
end;

function TTileDownloaderConfigStatic.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TTileDownloaderConfigStatic.GetExpectedMIMETypes: AnsiString;
begin
  Result := FExpectedMIMETypes;
end;

function TTileDownloaderConfigStatic.GetIgnoreMIMEType: Boolean;
begin
  Result := FIgnoreMIMEType;
end;

function TTileDownloaderConfigStatic.GetInetConfigStatic: IInetConfigStatic;
begin
  Result := FInetConfigStatic;
end;

function TTileDownloaderConfigStatic.GetMaxConnectToServerCount: Cardinal;
begin
  Result := FMaxConnectToServerCount;
end;

function TTileDownloaderConfigStatic.GetWaitInterval: Cardinal;
begin
  Result := FWaitInterval;
end;

function TTileDownloaderConfigStatic.GetRestartDownloadOnMemCacheTTL: Boolean;
begin
  Result := FRestartDownloadOnMemCacheTTL;
end;

end.
