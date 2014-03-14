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

unit u_ImageResamplerConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ImageResamplerFactory,
  i_ImageResamplerConfig,
  u_ConfigDataElementBase;

type
  TImageResamplerConfig = class(TConfigDataElementBase, IImageResamplerConfig)
  private
    FList: IImageResamplerFactoryList;
    FActiveIndex: Integer;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetList: IImageResamplerFactoryList;
    function GetActiveIndex: Integer;
    procedure SetActiveIndex(AValue: Integer);
    function GetActiveFactory: IImageResamplerFactory;
  public
    constructor Create(const AList: IImageResamplerFactoryList);
  end;

implementation

{ TMainFormMainConfig }

constructor TImageResamplerConfig.Create(const AList: IImageResamplerFactoryList);
begin
  inherited Create;
  FList := AList;
end;

procedure TImageResamplerConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FActiveIndex := AConfigData.ReadInteger('ResamplingType', FActiveIndex);
    if FActiveIndex < 0 then begin
      FActiveIndex := 0;
    end else if FActiveIndex >= FList.Count then begin
      FActiveIndex := FList.Count - 1;
    end;
    SetChanged;
  end;
end;

procedure TImageResamplerConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteInteger('ResamplingType', FActiveIndex);
end;

function TImageResamplerConfig.GetActiveFactory: IImageResamplerFactory;
begin
  LockRead;
  try
    Result := FList.Items[FActiveIndex];
  finally
    UnlockRead;
  end;
end;

function TImageResamplerConfig.GetActiveIndex: Integer;
begin
  LockRead;
  try
    Result := FActiveIndex;
  finally
    UnlockRead;
  end;
end;

function TImageResamplerConfig.GetList: IImageResamplerFactoryList;
begin
  Result := FList;
end;

procedure TImageResamplerConfig.SetActiveIndex(AValue: Integer);
begin
  LockWrite;
  try
    if FActiveIndex <> AValue then begin
      FActiveIndex := AValue;
      if FActiveIndex < 0 then begin
        FActiveIndex := 0;
      end else if FActiveIndex >= FList.Count then begin
        FActiveIndex := FList.Count - 1;
      end;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
