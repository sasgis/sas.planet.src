{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_TileRequestBuilderConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_TileRequestBuilderConfig,
  u_ConfigDataElementBase;

type
  TTileRequestBuilderConfigStatic = class(TInterfacedObject, ITileRequestBuilderConfigStatic)
  private
    FUrlBase: string;
    FRequestHeader: string;
  protected
    function  GetUrlBase: string;
    function  GetRequestHeader: string;
  public
    constructor Create(
      AUrlBase: string;
      ARequestHeader: string
    );
  end;

  TTileRequestBuilderConfig = class(TConfigDataElementBase, ITileRequestBuilderConfig)
  private
    FDefConfig: ITileRequestBuilderConfigStatic;
    FUrlBase: string;
    FRequestHeader: string;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function  GetUrlBase: string;
    procedure SetUrlBase(AValue: string);

    function  GetRequestHeader: string;
    procedure SetRequestHeader(AValue: string);
  public
    constructor Create(ADefConfig: ITileRequestBuilderConfigStatic);
  end;

implementation

uses
  SysUtils;

{ TTileRequestBuilderConfig }

constructor TTileRequestBuilderConfig.Create(ADefConfig: ITileRequestBuilderConfigStatic);
begin
  inherited Create;
  FDefConfig := ADefConfig;
  FUrlBase := FDefConfig.UrlBase;
  FRequestHeader := FDefConfig.RequestHeader;
end;

procedure TTileRequestBuilderConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetUrlBase(AConfigData.ReadString('URLBase', FUrlBase));
    SetRequestHeader(
      StringReplace(
        AConfigData.ReadString('RequestHead', FRequestHeader),
        '\r\n',
        #13#10,
        [rfIgnoreCase, rfReplaceAll]
      )
    );
  end;
end;

procedure TTileRequestBuilderConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  if FURLBase <> FDefConfig.UrlBase then begin
    AConfigData.WriteString('URLBase', FURLBase);
  end else begin
    AConfigData.DeleteValue('URLBase');
  end;

  if FRequestHeader <> FDefConfig.RequestHeader then begin
    AConfigData.WriteString(
      'RequestHead',
      StringReplace(
        FRequestHeader,
        #13#10,
        '\r\n',
        [rfIgnoreCase, rfReplaceAll]
      )
    );
  end else begin
    AConfigData.DeleteValue('RequestHead');
  end;
end;

function TTileRequestBuilderConfig.GetRequestHeader: string;
begin
  LockRead;
  try
    Result := FRequestHeader;
  finally
    UnlockRead;
  end;
end;

function TTileRequestBuilderConfig.GetUrlBase: string;
begin
  LockRead;
  try
    Result := FUrlBase;
  finally
    UnlockRead;
  end;
end;

procedure TTileRequestBuilderConfig.SetRequestHeader(AValue: string);
begin
  LockWrite;
  try
    if FRequestHeader <> AValue then begin
      FRequestHeader := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TTileRequestBuilderConfig.SetUrlBase(AValue: string);
begin
  LockWrite;
  try
    if FUrlBase <> AValue then begin
      FUrlBase := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

{ TTileRequestBuilderConfigStatic }

constructor TTileRequestBuilderConfigStatic.Create(AUrlBase,
  ARequestHeader: string);
begin
  FUrlBase := AUrlBase;
  FRequestHeader := ARequestHeader;
end;

function TTileRequestBuilderConfigStatic.GetRequestHeader: string;
begin
  Result := FRequestHeader;
end;

function TTileRequestBuilderConfigStatic.GetUrlBase: string;
begin
  Result := FUrlBase;
end;

end.
