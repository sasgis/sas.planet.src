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

unit u_GlobalDownloadConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GlobalDownloadConfig,
  u_ConfigDataElementBase;

type
  TGlobalDownloadConfig = class(TConfigDataElementBase, IGlobalDownloadConfig)
  private
    FIsGoNextTileIfDownloadError: Boolean;
    FIsUseSessionLastSuccess: Boolean;
    FIsSaveTileNotExists: Boolean;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetIsGoNextTileIfDownloadError: Boolean;
    procedure SetIsGoNextTileIfDownloadError(AValue: Boolean);

    function GetIsUseSessionLastSuccess: Boolean;
    procedure SetIsUseSessionLastSuccess(AValue: Boolean);

    function GetIsSaveTileNotExists: Boolean;
    procedure SetIsSaveTileNotExists(AValue: Boolean);
  public
    constructor Create;
  end;

implementation

{ TGlobalDownloadConfig }

constructor TGlobalDownloadConfig.Create;
begin
  inherited Create;
  FIsGoNextTileIfDownloadError := True;
  FIsUseSessionLastSuccess := True;
  FIsSaveTileNotExists := False;
end;

procedure TGlobalDownloadConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FIsGoNextTileIfDownloadError := AConfigData.ReadBool('GoNextTile', FIsGoNextTileIfDownloadError);
    FIsUseSessionLastSuccess := AConfigData.ReadBool('SessionLastSuccess', FIsUseSessionLastSuccess);
    FIsSaveTileNotExists := AConfigData.ReadBool('SaveTNE', FIsSaveTileNotExists);
    SetChanged;
  end;
end;

procedure TGlobalDownloadConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('GoNextTile', FIsGoNextTileIfDownloadError);
  AConfigData.WriteBool('SessionLastSuccess', FIsUseSessionLastSuccess);
  AConfigData.WriteBool('SaveTNE', FIsSaveTileNotExists);
end;

function TGlobalDownloadConfig.GetIsGoNextTileIfDownloadError: Boolean;
begin
  LockRead;
  try
    Result := FIsGoNextTileIfDownloadError;
  finally
    UnlockRead;
  end;
end;

function TGlobalDownloadConfig.GetIsSaveTileNotExists: Boolean;
begin
  LockRead;
  try
    Result := FIsSaveTileNotExists;
  finally
    UnlockRead;
  end;
end;

function TGlobalDownloadConfig.GetIsUseSessionLastSuccess: Boolean;
begin
  LockRead;
  try
    Result := FIsUseSessionLastSuccess;
  finally
    UnlockRead;
  end;
end;

procedure TGlobalDownloadConfig.SetIsGoNextTileIfDownloadError(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsGoNextTileIfDownloadError <> AValue then begin
      FIsGoNextTileIfDownloadError := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGlobalDownloadConfig.SetIsSaveTileNotExists(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsSaveTileNotExists <> AValue then begin
      FIsSaveTileNotExists := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGlobalDownloadConfig.SetIsUseSessionLastSuccess(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsUseSessionLastSuccess <> AValue then begin
      FIsUseSessionLastSuccess := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
