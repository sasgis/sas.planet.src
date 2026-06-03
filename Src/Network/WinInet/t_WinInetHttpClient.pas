{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit t_WinInetHttpClient;

interface

uses
  Windows,
  Classes,
  WinInet;

type
  TWinInetReqMethod = (rmGet, rmPost, rmHead);

  TWinInetProxy = record
    AccessType: DWORD;
    Host: RawByteString;
    UserName: AnsiString;
    Password: AnsiString;
    class operator Equal(const A, B: TWinInetProxy): Boolean; inline;
    class operator NotEqual(const A, B: TWinInetProxy): Boolean; inline;
  end;
  PWinInetProxy = ^TWinInetProxy;

  TWinInetOptions = record
    TimeOutMS: DWORD;
    Flags: DWORD;
    IgnoreSecurityErrors: Boolean;
    AllowHttp2Protocol: Boolean;
    class operator Equal(const A, B: TWinInetOptions): Boolean; inline;
    class operator NotEqual(const A, B: TWinInetOptions): Boolean; inline;
  end;
  PWinInetOptions = ^TWinInetOptions;

  TWinInetRequest = record
    Method: TWinInetReqMethod;
    Url: RawByteString;
    UserAgent: RawByteString;
    Headers: RawByteString;
    PostData: Pointer;
    PostDataSize: NativeInt;
    Proxy: PWinInetProxy;
    Options: PWinInetOptions;
  end;
  PWinInetRequest = ^TWinInetRequest;

  TWinInetResponse = record
    Code: DWORD;
    Headers: RawByteString;
    Data: TMemoryStream;
    ErrorReason: string;
  end;
  PWinInetResponse = ^TWinInetResponse;

  TWinInetProgressCallBack = procedure(const ATotal, ADownload: Integer) of object;
  TWinInetStatusCallBack = procedure(const AStatus: DWORD; const AInfo: Pointer; const AInfoLen: DWORD) of object;

implementation

{ TWinInetProxy }

class operator TWinInetProxy.Equal(const A, B: TWinInetProxy): Boolean;
begin
  Result :=
    (A.AccessType = B.AccessType) and
    (A.Host = B.Host) and
    (A.UserName = B.UserName) and
    (A.Password = B.Password);
end;

class operator TWinInetProxy.NotEqual(const A, B: TWinInetProxy): Boolean;
begin
  Result := not (A = B);
end;

{ TWinInetOptions }

class operator TWinInetOptions.Equal(const A, B: TWinInetOptions): Boolean;
begin
  Result :=
    (A.TimeOutMS = B.TimeOutMS) and
    (A.Flags = B.Flags) and
    (A.IgnoreSecurityErrors = B.IgnoreSecurityErrors) and
    (A.AllowHttp2Protocol = B.AllowHttp2Protocol);
end;

class operator TWinInetOptions.NotEqual(const A, B: TWinInetOptions): Boolean;
begin
  Result := not (A = B);
end;

end.
