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

unit u_IeEmbeddedProtocolRegistration;

interface

uses
  UrlMon,
  ActiveX;

type
  TIeEmbeddedProtocolRegistration = class
  private
    FProtocol: WideString;
    FFactory: IClassFactory;
    FInternetSession: IInternetSession;
  public
    constructor Create(
      const AProtocol: PWideChar;
      const AFactory: IClassFactory
    );
    destructor Destroy; override;
  end;

implementation

const
  CIEEmbeddedProtocol_Class: TGUID = '{A9CA884C-253A-4662-A4F6-6926BAB877F9}';

{ TIeEmbeddedProtocolRegistration }

constructor TIeEmbeddedProtocolRegistration.Create(
  const AProtocol: PWideChar;
  const AFactory: IClassFactory
);
begin
  inherited Create;
  FProtocol := AProtocol;
  FFactory := AFactory;
  CoInternetGetSession(0, FInternetSession, 0);
  FInternetSession.RegisterNameSpace(FFactory, CIEEmbeddedProtocol_Class, PWideChar(FProtocol), 0, nil, 0);
end;

destructor TIeEmbeddedProtocolRegistration.Destroy;
begin
  if Assigned(FInternetSession) then begin
    FInternetSession.UnregisterNameSpace(FFactory, PWideChar(FProtocol));
  end;
  FFactory := nil;
  FInternetSession := nil;
  inherited;
end;

end.
