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

unit u_BitmapPostProcessingConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_BitmapPostProcessingConfig,
  u_ConfigDataElementBase;

type
  TBitmapPostProcessingConfig = class(TConfigDataElementWithStaticBase, IBitmapPostProcessingConfig)
  private
    FInvertColor: boolean;
    FGammaN: Integer;
    FContrastN: Integer;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
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
  inherited Create;
  FInvertColor := False;
  FContrastN := 0;
  FGammaN := 50;
end;

function TBitmapPostProcessingConfig.CreateStatic: IInterface;
var
  VStatic: IBitmapPostProcessingConfigStatic;
begin
  VStatic :=
    TBitmapPostProcessingConfigStatic.Create(
      FInvertColor,
      FGammaN,
      FContrastN
    );
  Result := VStatic;
end;

procedure TBitmapPostProcessingConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
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
  const AConfigData: IConfigDataWriteProvider
);
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
  Result := IBitmapPostProcessingConfigStatic(GetStaticInternal);
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
