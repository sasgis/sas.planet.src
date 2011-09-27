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
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
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
  inherited;
  FIsGoNextTileIfDownloadError := True;
  FIsUseSessionLastSuccess := True;
  FIsSaveTileNotExists := True;
end;

procedure TGlobalDownloadConfig.DoReadConfig(AConfigData: IConfigDataProvider);
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
  AConfigData: IConfigDataWriteProvider);
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
