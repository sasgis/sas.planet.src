{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_TileDownloadRequestBuilderConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_TileDownloadRequestBuilderConfig,
  u_ConfigDataElementBase,
  u_BaseInterfacedObject;

type
  TTileDownloadRequestBuilderConfigStatic = class(TBaseInterfacedObject, ITileDownloadRequestBuilderConfigStatic)
  private
    FUrlBase: AnsiString;
    FRequestHeader: AnsiString;
    FIsUseDownloader: Boolean;
    FDefaultProjConverterArgs: AnsiString;
  private
    function GetUrlBase: AnsiString;
    function GetRequestHeader: AnsiString;
    function GetIsUseDownloader: Boolean;
    function GetDefaultProjConverterArgs: AnsiString;
  public
    constructor Create(
      const AUrlBase: AnsiString;
      const ARequestHeader: AnsiString;
      const AIsUseDownloader: Boolean;
      const ADefaultProjConverterArgs: AnsiString
    );
  end;

  TTileDownloadRequestBuilderConfig = class(TConfigDataElementBase, ITileDownloadRequestBuilderConfig)
  private
    FDefConfig: ITileDownloadRequestBuilderConfigStatic;
    FUrlBase: AnsiString;
    FRequestHeader: AnsiString;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetUrlBase: AnsiString;
    procedure SetUrlBase(const AValue: AnsiString);

    function GetRequestHeader: AnsiString;
    procedure SetRequestHeader(const AValue: AnsiString);

    function GetIsUseDownloader: Boolean;
    function GetDefaultProjConverterArgs: AnsiString;
  public
    constructor Create(const ADefConfig: ITileDownloadRequestBuilderConfigStatic);
  end;

implementation

uses
  SysUtils,
  ALString;

{ TTileDownloadRequestBuilderConfig }

constructor TTileDownloadRequestBuilderConfig.Create(
  const ADefConfig: ITileDownloadRequestBuilderConfigStatic
);
begin
  inherited Create;
  FDefConfig := ADefConfig;
  FUrlBase := FDefConfig.UrlBase;
  FRequestHeader := FDefConfig.RequestHeader;
end;

procedure TTileDownloadRequestBuilderConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
var
  VRequestHeader: AnsiString;
begin
  inherited;
  if AConfigData <> nil then begin
    SetUrlBase(AConfigData.ReadAnsiString('URLBase', FUrlBase));
    VRequestHeader :=
      ALStringReplace(
        AConfigData.ReadAnsiString('RequestHead', FRequestHeader),
        '\r\n',
        #13#10,
        [rfIgnoreCase, rfReplaceAll]
      );
    SetRequestHeader(VRequestHeader);
  end;
end;

procedure TTileDownloadRequestBuilderConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
var
  VRequestHeader: AnsiString;
begin
  inherited;
  if FUrlBase <> FDefConfig.UrlBase then begin
    AConfigData.WriteAnsiString('URLBase', FUrlBase);
  end else begin
    AConfigData.DeleteValue('URLBase');
  end;

  if FRequestHeader <> FDefConfig.RequestHeader then begin
    VRequestHeader :=
      ALStringReplace(
        FRequestHeader,
        #13#10,
        '\r\n',
        [rfIgnoreCase, rfReplaceAll]
      );
    AConfigData.WriteAnsiString('RequestHead', VRequestHeader);
  end else begin
    AConfigData.DeleteValue('RequestHead');
  end;
end;

function TTileDownloadRequestBuilderConfig.GetDefaultProjConverterArgs: AnsiString;
begin
  Result := FDefConfig.DefaultProjConverterArgs;
end;

function TTileDownloadRequestBuilderConfig.GetIsUseDownloader: Boolean;
begin
  Result := FDefConfig.IsUseDownloader;
end;

function TTileDownloadRequestBuilderConfig.GetRequestHeader: AnsiString;
begin
  LockRead;
  try
    Result := FRequestHeader;
  finally
    UnlockRead;
  end;
end;

function TTileDownloadRequestBuilderConfig.GetUrlBase: AnsiString;
begin
  LockRead;
  try
    Result := FUrlBase;
  finally
    UnlockRead;
  end;
end;

procedure TTileDownloadRequestBuilderConfig.SetRequestHeader(const AValue: AnsiString);
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

procedure TTileDownloadRequestBuilderConfig.SetUrlBase(const AValue: AnsiString);
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

{ TTileDownloadRequestBuilderConfigStatic }

constructor TTileDownloadRequestBuilderConfigStatic.Create(
  const AUrlBase, ARequestHeader: AnsiString;
  const AIsUseDownloader: Boolean;
  const ADefaultProjConverterArgs: AnsiString
);
begin
  inherited Create;
  FUrlBase := AUrlBase;
  FRequestHeader := ARequestHeader;
  FIsUseDownloader := AIsUseDownloader;
  FDefaultProjConverterArgs := ADefaultProjConverterArgs;
end;

function TTileDownloadRequestBuilderConfigStatic.GetDefaultProjConverterArgs: AnsiString;
begin
  Result := FDefaultProjConverterArgs;
end;

function TTileDownloadRequestBuilderConfigStatic.GetIsUseDownloader: Boolean;
begin
  Result := FIsUseDownloader;
end;

function TTileDownloadRequestBuilderConfigStatic.GetRequestHeader: AnsiString;
begin
  Result := FRequestHeader;
end;

function TTileDownloadRequestBuilderConfigStatic.GetUrlBase: AnsiString;
begin
  Result := FUrlBase;
end;

end.
