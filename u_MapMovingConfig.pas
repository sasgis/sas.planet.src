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

unit u_MapMovingConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapMovingConfig,
  u_ConfigDataElementBase;

type
  TMapMovingConfig = class(TConfigDataElementBase, IMapMovingConfig)
  private
    FAnimateMove: Boolean;
    FAnimateMoveTime: Cardinal;
    FAnimateMaxStartSpeed: Cardinal;
    FAnimateMinStartSpeed: Cardinal;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetAnimateMove: Boolean;
    procedure SetAnimateMove(AValue: Boolean);

    function GetAnimateMoveTime: Cardinal;
    procedure SetAnimateMoveTime(AValue: Cardinal);

    function GetAnimateMaxStartSpeed: Cardinal;
    procedure SetAnimateMaxStartSpeed(AValue: Cardinal);

    function GetAnimateMinStartSpeed: Cardinal;
    procedure SetAnimateMinStartSpeed(AValue: Cardinal);
  public
    constructor Create;
  end;

implementation

{ TMapMovingConfig }

constructor TMapMovingConfig.Create;
begin
  inherited Create;
  FAnimateMove := True;
  FAnimateMoveTime := 600;
  FAnimateMaxStartSpeed := 4000;
  FAnimateMinStartSpeed := 100;
end;

procedure TMapMovingConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FAnimateMove := AConfigData.ReadBool('AnimateMove', FAnimateMove);
    FAnimateMoveTime := AConfigData.ReadInteger('AnimateMoveTime', FAnimateMoveTime);
    FAnimateMaxStartSpeed := AConfigData.ReadInteger('AnimateMaxStartSpeed', FAnimateMaxStartSpeed);
    FAnimateMinStartSpeed := AConfigData.ReadInteger('AnimateMinStartSpeed', FAnimateMinStartSpeed);
    SetChanged;
  end;
end;

procedure TMapMovingConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('AnimateMove', FAnimateMove);
  AConfigData.WriteInteger('AnimateMoveTime', FAnimateMoveTime);
  AConfigData.WriteInteger('AnimateMaxStartSpeed', FAnimateMaxStartSpeed);
  AConfigData.WriteInteger('AnimateMinStartSpeed', FAnimateMinStartSpeed);
end;

function TMapMovingConfig.GetAnimateMove: Boolean;
begin
  LockRead;
  try
    Result := FAnimateMove;
  finally
    UnlockRead;
  end;
end;

function TMapMovingConfig.GetAnimateMoveTime: Cardinal;
begin
  LockRead;
  try
    Result := FAnimateMoveTime;
  finally
    UnlockRead;
  end;
end;

function TMapMovingConfig.GetAnimateMaxStartSpeed: Cardinal;
begin
  LockRead;
  try
    Result := FAnimateMaxStartSpeed;
  finally
    UnlockRead;
  end;
end;

function TMapMovingConfig.GetAnimateMinStartSpeed: Cardinal;
begin
  LockRead;
  try
    Result := FAnimateMinStartSpeed;
  finally
    UnlockRead;
  end;
end;

procedure TMapMovingConfig.SetAnimateMove(AValue: Boolean);
begin
  LockWrite;
  try
    if FAnimateMove <> AValue then begin
      FAnimateMove := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapMovingConfig.SetAnimateMoveTime(AValue: Cardinal);
begin
  LockWrite;
  try
    if FAnimateMoveTime <> AValue then begin
      FAnimateMoveTime := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapMovingConfig.SetAnimateMaxStartSpeed(AValue: Cardinal);
begin
  LockWrite;
  try
    if FAnimateMaxStartSpeed <> AValue then begin
      FAnimateMaxStartSpeed := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapMovingConfig.SetAnimateMinStartSpeed(AValue: Cardinal);
begin
  LockWrite;
  try
    if FAnimateMinStartSpeed <> AValue then begin
      FAnimateMinStartSpeed := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
