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

unit u_BitmapPostProcessingConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_BitmapPostProcessingConfig,
  u_ConfigDataElementBase;

type
  TBitmapPostProcessingConfig = class(TConfigDataElementBase, IBitmapPostProcessingConfig)
  private
    FInvertColor: boolean;
    FGammaN: Integer;
    FContrastN: Integer;
    FStatic: IBitmapPostProcessingConfigStatic;
    function CreateStatic: IBitmapPostProcessingConfigStatic;
  protected
    procedure DoBeforeChangeNotify; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetInvertColor: boolean;
    procedure SetInvertColor(const AValue: boolean);
    function GetGammaN: Integer;
    procedure SetGammaN(const AValue: Integer);
    function GetContrastN: Integer;
    procedure SetContrastN(const AValue: Integer);
    function GetStatic: IBitmapPostProcessingConfigStatic;
  public
    constructor Create;
  end;

implementation

uses
  u_BitmapPostProcessingConfigStatic;

{ TBitmapPostProcessingConfig }

constructor TBitmapPostProcessingConfig.Create;
begin
  inherited;
  FInvertColor := False;
  FContrastN := 0;
  FGammaN := 50;
  FStatic := TBitmapPostProcessingConfigStatic.Create(FInvertColor, FGammaN, FContrastN);
end;

function TBitmapPostProcessingConfig.CreateStatic: IBitmapPostProcessingConfigStatic;
begin
  Result :=
    TBitmapPostProcessingConfigStatic.Create(
      FInvertColor,
      FGammaN,
      FContrastN
    );
end;

procedure TBitmapPostProcessingConfig.DoBeforeChangeNotify;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapPostProcessingConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FInvertColor := AConfigData.ReadBool('InvertColor', FInvertColor);
    FGammaN := AConfigData.ReadInteger('Gamma', FGammaN);
    FContrastN := AConfigData.ReadInteger('Contrast', FContrastN);
    SetChanged;
  end;
end;

procedure TBitmapPostProcessingConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('InvertColor', FInvertColor);
  AConfigData.WriteInteger('Gamma', FGammaN);
  AConfigData.WriteInteger('Contrast', FContrastN);
end;

function TBitmapPostProcessingConfig.GetContrastN: Integer;
begin
  LockRead;
  try
    Result := FContrastN;
  finally
    UnlockRead;
  end;
end;

function TBitmapPostProcessingConfig.GetGammaN: Integer;
begin
  LockRead;
  try
    Result := FGammaN;
  finally
    UnlockRead;
  end;
end;

function TBitmapPostProcessingConfig.GetInvertColor: boolean;
begin
  LockRead;
  try
    Result := FInvertColor;
  finally
    UnlockRead;
  end;
end;

function TBitmapPostProcessingConfig.GetStatic: IBitmapPostProcessingConfigStatic;
begin
  LockRead;
  try
    Result := FStatic;
  finally
    UnlockRead;
  end;
end;

procedure TBitmapPostProcessingConfig.SetContrastN(const AValue: Integer);
begin
  LockWrite;
  try
    if FContrastN <> AValue then begin
      FContrastN := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapPostProcessingConfig.SetGammaN(const AValue: Integer);
begin
  LockWrite;
  try
    if FGammaN <> AValue then begin
      FGammaN := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapPostProcessingConfig.SetInvertColor(const AValue: boolean);
begin
  LockWrite;
  try
    if FInvertColor <> AValue then begin
      FInvertColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
