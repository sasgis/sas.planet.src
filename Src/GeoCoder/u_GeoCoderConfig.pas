{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit u_GeoCoderConfig;

interface

uses
  i_PathConfig,
  i_GeoCoderConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase;

type
  TGeoCoderConfig = class(TConfigDataElementBase, IGeoCoderConfig)
  private
    FUserDataPath: IPathConfig;

    FGoogleApiKey: string;
    FYandexApiKey: string;

    function ReadApiKey(const AFileName: string): string;
    procedure WriteApiKey(const AFileName: string; const AKey: string);
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { IGeoCoderConfig }
    function GetDataPath: string;

    function GetGoogleApiKey: string;
    procedure SetGoogleApiKey(const AValue: string);

    function GetYandexApiKey: string;
    procedure SetYandexApiKey(const AValue: string);
  public
    constructor Create(
      const AUserDataPath: IPathConfig
    );
  end;

implementation

uses
  Classes,
  SysUtils;

const
  CGoogleApiKeyFileName = 'GoogleApiKey.txt';
  CYandexApiKeyFileName = 'YandexApiKey.txt';

{ TGeoCoderConfig }

constructor TGeoCoderConfig.Create(const AUserDataPath: IPathConfig);
begin
  inherited Create;

  FUserDataPath := AUserDataPath;

  FGoogleApiKey := '';
  FYandexApiKey := '';
end;

procedure TGeoCoderConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  LockWrite;
  try
    FGoogleApiKey := ReadApiKey(CGoogleApiKeyFileName);
    FYandexApiKey := ReadApiKey(CYandexApiKeyFileName);
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TGeoCoderConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  // nothing here
end;

function TGeoCoderConfig.GetDataPath: string;
begin
  Result := IncludeTrailingPathDelimiter(FUserDataPath.FullPath);
end;

function TGeoCoderConfig.GetGoogleApiKey: string;
begin
  LockRead;
  try
    Result := FGoogleApiKey;
  finally
    UnlockRead;
  end;
end;

procedure TGeoCoderConfig.SetGoogleApiKey(const AValue: string);
begin
  LockWrite;
  try
    if FGoogleApiKey <> AValue then begin
      FGoogleApiKey := AValue;
      WriteApiKey(CGoogleApiKeyFileName, FGoogleApiKey);
    end;
  finally
    UnlockWrite;
  end;
end;

function TGeoCoderConfig.GetYandexApiKey: string;
begin
  LockRead;
  try
    Result := FYandexApiKey;
  finally
    UnlockRead;
  end;
end;

procedure TGeoCoderConfig.SetYandexApiKey(const AValue: string);
begin
  LockWrite;
  try
    if FYandexApiKey <> AValue then begin
      FYandexApiKey := AValue;
      WriteApiKey(CYandexApiKeyFileName, FYandexApiKey);
    end;
  finally
    UnlockWrite;
  end;
end;

function TGeoCoderConfig.ReadApiKey(const AFileName: string): string;
var
  I: Integer;
  VList: TStringList;
  VFileName: string;
begin
  Result := '';

  VFileName := Self.GetDataPath + AFileName;

  if not FileExists(VFileName) then begin
    Exit;
  end;

  VList := TStringList.Create;
  try
    VList.LoadFromFile(VFileName);
    for I := 0 to VList.Count - 1 do begin
      Result := Trim(VList.Strings[I]);
      if Result <> '' then begin
        Break;
      end;
    end;
  finally
    VList.Free;
  end;
end;

procedure TGeoCoderConfig.WriteApiKey(const AFileName, AKey: string);
var
  VStream: TMemoryStream;
  VFileName: string;
  VKey: UTF8String;
begin
  VFileName := Self.GetDataPath + AFileName;

  if not ForceDirectories(ExtractFileDir(VFileName)) then begin
    RaiseLastOSError;
  end;

  VStream := TMemoryStream.Create;
  try
    VKey := UTF8Encode(AKey);
    VStream.WriteBuffer(VKey[1], Length(VKey));
    VStream.SaveToFile(VFileName);
  finally
    VStream.Free;
  end;
end;

end.
