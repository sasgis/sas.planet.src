{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
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
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
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
  inherited;
  FAnimateMove := True;
  FAnimateMoveTime := 600;
  FAnimateMaxStartSpeed := 4000;
  FAnimateMinStartSpeed := 100;
end;

procedure TMapMovingConfig.DoReadConfig(AConfigData: IConfigDataProvider);
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
  AConfigData: IConfigDataWriteProvider);
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
