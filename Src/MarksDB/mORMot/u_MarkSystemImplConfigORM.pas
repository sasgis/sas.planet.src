{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_MarkSystemImplConfigORM;

interface

uses
  SynCommons,
  i_MarkSystemImplConfigORM,
  u_MarkSystemImplConfigBase;

type
  TMarkSystemImplConfigORM = class(TMarkSystemImplConfigBase, IMarkSystemImplConfigORM)
  private
    FSynUser: TSynUserPassword;
    FCacheSizeMb: Cardinal;
  private
    { IMarkSystemImplConfigORM }
    function GetUserName: string;
    function GetPassword: string;
    function GetPasswordPlain: string;
    function GetCacheSizeMb: Cardinal;
  public
    constructor Create(
      const AFileName: string;
      const AIsReadOnly: Boolean;
      const AUserName: string;
      const APasswordPlain: string;
      const APassword: string;
      const ACacheSizeMb: Cardinal = 100
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TMarkSystemImplConfigORM }

constructor TMarkSystemImplConfigORM.Create(
  const AFileName: string;
  const AIsReadOnly: Boolean;
  const AUserName: string;
  const APasswordPlain: string;
  const APassword: string;
  const ACacheSizeMb: Cardinal
);
begin
  inherited Create(AFileName, AIsReadOnly);
  FCacheSizeMb := ACacheSizeMb;
  FSynUser := TSynUserPassword.Create;
  FSynUser.UserName := StringToUTF8(AUserName);
  if APasswordPlain <> '' then begin
    FSynUser.PasswordPlain := StringToUTF8(APasswordPlain);
  end else begin
    FSynUser.Password := StringToUTF8(APassword);
  end;
end;

destructor TMarkSystemImplConfigORM.Destroy;
begin
  FreeAndNil(FSynUser);
  inherited Destroy;
end;

function TMarkSystemImplConfigORM.GetUserName: string;
begin
  Result := UTF8ToString(FSynUser.UserName);
end;

function TMarkSystemImplConfigORM.GetPassword: string;
begin
  Result := UTF8ToString(FSynUser.Password);
end;

function TMarkSystemImplConfigORM.GetPasswordPlain: string;
begin
  Result := UTF8ToString(FSynUser.PasswordPlain);
end;

function TMarkSystemImplConfigORM.GetCacheSizeMb: Cardinal;
begin
  Result := FCacheSizeMb;
end;

end.
