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
  i_TileDownloaderConfig;

type
  TTileDownloaderConfigStatic = class(TInterfacedObject, ITileDownloaderConfigStatic)
  private
    FInetConfigStatic: IInetConfigStatic;
    FEnabled: Boolean;
    FWaitInterval: Cardinal;
    FMaxConnectToServerCount: Cardinal;
    FIgnoreMIMEType: Boolean;
    FExpectedMIMETypes: string;
    FDefaultMIMEType: string;
    FIteratorSubRectSize: TPoint;
  protected
    function GetInetConfigStatic: IInetConfigStatic;
    function GetEnabled: Boolean;
    function GetWaitInterval: Cardinal;
    function GetMaxConnectToServerCount: Cardinal;
    function GetIgnoreMIMEType: Boolean;
    function GetExpectedMIMETypes: string;
    function GetDefaultMIMEType: string;
    function GetIteratorSubRectSize: TPoint;
  public
    constructor Create(
      const AInetConfigStatic: IInetConfigStatic;
      AEnabled: Boolean;
      AWaitInterval: Cardinal;
      AMaxConnectToServerCount: Cardinal;
      AIgnoreMIMEType: Boolean;
      const AExpectedMIMETypes: string;
      const ADefaultMIMEType: string;
      const AIteratorSubRectSize: TPoint
    );
  end;

implementation

{ TTileDownloaderConfigStatic }

constructor TTileDownloaderConfigStatic.Create(
  const AInetConfigStatic: IInetConfigStatic;
  AEnabled: Boolean;
  AWaitInterval: Cardinal;
  AMaxConnectToServerCount: Cardinal;
  AIgnoreMIMEType: Boolean;
  const AExpectedMIMETypes, ADefaultMIMEType: string;
  const AIteratorSubRectSize: TPoint
);
begin
  FInetConfigStatic := AInetConfigStatic;
  FEnabled := AEnabled;
  FWaitInterval := AWaitInterval;
  FMaxConnectToServerCount := AMaxConnectToServerCount;
  FIgnoreMIMEType := AIgnoreMIMEType;
  FExpectedMIMETypes := AExpectedMIMETypes;
  FDefaultMIMEType := ADefaultMIMEType;
  FIteratorSubRectSize := AIteratorSubRectSize;
end;

function TTileDownloaderConfigStatic.GetIteratorSubRectSize: TPoint;
begin
  Result := FIteratorSubRectSize;
end;

function TTileDownloaderConfigStatic.GetDefaultMIMEType: string;
begin
  Result := FDefaultMIMEType;
end;

function TTileDownloaderConfigStatic.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TTileDownloaderConfigStatic.GetExpectedMIMETypes: string;
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

end.
