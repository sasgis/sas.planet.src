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

unit u_TileDownloadRequestBuilderConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_CoordConverter,
  i_TileDownloadRequestBuilderConfig,
  u_ConfigDataElementBase;

type
  TTileDownloadRequestBuilderConfigStatic = class(TInterfacedObject, ITileDownloadRequestBuilderConfigStatic)
  private
    FUrlBase: string;
    FRequestHeader: string;
    FIsUseDownloader: Boolean;
    FDefaultProjConverterArgs: string;
    FGeoCoder: ICoordConverter;
  private
    function GetUrlBase: string;
    function GetRequestHeader: string;
    function GetIsUseDownloader: Boolean;
    function GetDefaultProjConverterArgs: string;
    function GetGeoCoder: ICoordConverter;
  public
    constructor Create(
      const AUrlBase: string;
      const ARequestHeader: string;
      const AIsUseDownloader: Boolean;
      const ADefaultProjConverterArgs: string;
      const AGeoCoder: ICoordConverter
    );
  end;

  TTileDownloadRequestBuilderConfig = class(TConfigDataElementBase, ITileDownloadRequestBuilderConfig)
  private
    FDefConfig: ITileDownloadRequestBuilderConfigStatic;
    FUrlBase: string;
    FRequestHeader: string;
    FGeoCoder: ICoordConverter;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetUrlBase: string;
    procedure SetUrlBase(const AValue: string);

    function GetRequestHeader: string;
    procedure SetRequestHeader(const AValue: string);

    function GetIsUseDownloader: Boolean;
    function GetDefaultProjConverterArgs: string;
    function GetGeoCoder: ICoordConverter;
  public
    constructor Create(const ADefConfig: ITileDownloadRequestBuilderConfigStatic);
  end;

implementation

uses
  SysUtils;

{ TTileDownloadRequestBuilderConfig }

constructor TTileDownloadRequestBuilderConfig.Create(
  const ADefConfig: ITileDownloadRequestBuilderConfigStatic
);
begin
  inherited Create;
  FDefConfig := ADefConfig;
  FUrlBase := FDefConfig.UrlBase;
  FRequestHeader := FDefConfig.RequestHeader;
  FGeoCoder := ADefConfig.GeoCoder;
end;

procedure TTileDownloadRequestBuilderConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
var
  VRequestHeader: string;
begin
  inherited;
  if AConfigData <> nil then begin
    SetUrlBase(AConfigData.ReadString('URLBase', FUrlBase));
    VRequestHeader :=
      StringReplace(
        AConfigData.ReadString('RequestHead', FRequestHeader),
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
  VRequestHeader: string;
begin
  inherited;
  if FUrlBase <> FDefConfig.UrlBase then begin
    AConfigData.WriteString('URLBase', FUrlBase);
  end else begin
    AConfigData.DeleteValue('URLBase');
  end;

  if FRequestHeader <> FDefConfig.RequestHeader then begin
    VRequestHeader :=
      StringReplace(
        FRequestHeader,
        #13#10,
        '\r\n',
        [rfIgnoreCase, rfReplaceAll]
      );
    AConfigData.WriteString('RequestHead', VRequestHeader);
  end else begin
    AConfigData.DeleteValue('RequestHead');
  end;
end;

function TTileDownloadRequestBuilderConfig.GetDefaultProjConverterArgs: string;
begin
  Result := FDefConfig.DefaultProjConverterArgs;
end;

function TTileDownloadRequestBuilderConfig.GetGeoCoder: ICoordConverter;
begin
  Result := FGeoCoder;
end;

function TTileDownloadRequestBuilderConfig.GetIsUseDownloader: Boolean;
begin
  Result := FDefConfig.IsUseDownloader;
end;

function TTileDownloadRequestBuilderConfig.GetRequestHeader: string;
begin
  LockRead;
  try
    Result := FRequestHeader;
  finally
    UnlockRead;
  end;
end;

function TTileDownloadRequestBuilderConfig.GetUrlBase: string;
begin
  LockRead;
  try
    Result := FUrlBase;
  finally
    UnlockRead;
  end;
end;

procedure TTileDownloadRequestBuilderConfig.SetRequestHeader(const AValue: string);
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

procedure TTileDownloadRequestBuilderConfig.SetUrlBase(const AValue: string);
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
  const AUrlBase, ARequestHeader: string;
  const AIsUseDownloader: Boolean;
  const ADefaultProjConverterArgs: string;
  const AGeoCoder: ICoordConverter
);
begin
  inherited Create;
  FUrlBase := AUrlBase;
  FRequestHeader := ARequestHeader;
  FIsUseDownloader := AIsUseDownloader;
  FDefaultProjConverterArgs := ADefaultProjConverterArgs;
  FGeoCoder := AGeoCoder;
end;

function TTileDownloadRequestBuilderConfigStatic.GetDefaultProjConverterArgs: string;
begin
  Result := FDefaultProjConverterArgs;
end;

function TTileDownloadRequestBuilderConfigStatic.GetGeoCoder: ICoordConverter;
begin
  Result := FGeoCoder;
end;

function TTileDownloadRequestBuilderConfigStatic.GetIsUseDownloader: Boolean;
begin
  Result := FIsUseDownloader;
end;

function TTileDownloadRequestBuilderConfigStatic.GetRequestHeader: string;
begin
  Result := FRequestHeader;
end;

function TTileDownloadRequestBuilderConfigStatic.GetUrlBase: string;
begin
  Result := FUrlBase;
end;

end.
