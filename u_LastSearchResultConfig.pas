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

unit u_LastSearchResultConfig;

interface

uses
  i_LastSearchResultConfig,
  i_GeoCoder,
  u_ConfigDataElementBase;

type
  TLastSearchResultConfig = class(TConfigDataElementBaseEmptySaveLoad, ILastSearchResultConfig)
  private
    FIsActive: Boolean;
    FGeoCodeResult: IGeoCodeResult;
  protected
    function GetIsActive: Boolean;
    function GetGeoCodeResult:IGeoCodeResult;
    procedure SetGeoCodeResult(const AValue: IGeoCodeResult);
    procedure ClearGeoCodeResult;
  end;

implementation

function TLastSearchResultConfig.GetGeoCodeResult:IGeoCodeResult;
begin
  LockRead;
  try
    Result := FGeoCodeResult;
  finally
    UnlockRead;
  end;
end;

function TLastSearchResultConfig.GetIsActive: Boolean;
begin
  LockRead;
  try
    Result := FIsActive;
  finally
    UnlockRead;
  end;
end;

procedure TLastSearchResultConfig.SetGeoCodeResult(const AValue: IGeoCodeResult);
begin
  LockWrite;
  try
    if FGeoCodeResult <> AValue then begin
      FIsActive := True;
      FGeoCodeResult:=AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TLastSearchResultConfig.ClearGeoCodeResult;
begin
  LockWrite;
  try
    FIsActive := false;
    FGeoCodeResult:=nil;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;


end.
