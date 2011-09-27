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

unit u_InetConfigStatic;

interface

uses
  i_ProxySettings,
  i_InetConfig;

type
  TInetConfigStatic = class(TInterfacedObject, IInetConfigStatic)
  private
    FProxyConfigStatic: IProxyConfigStatic;
    FUserAgentString: string;
    FTimeOut: Cardinal;
    FSleepOnResetConnection: Cardinal;
    FDownloadTryCount: Integer;
  protected
    function GetProxyConfigStatic: IProxyConfigStatic;
    function GetUserAgentString: string;
    function GetTimeOut: Cardinal;
    function GetSleepOnResetConnection: Cardinal;
    function GetDownloadTryCount: Integer;
  public
    constructor Create(
      AProxyConfigStatic: IProxyConfigStatic;
      AUserAgentString: string;
      ATimeOut: Cardinal;
      ASleepOnResetConnection: Cardinal;
      ADownloadTryCount: Integer
    );
  end;

implementation

{ TTileDownloaderConfigStatic }

constructor TInetConfigStatic.Create(
  AProxyConfigStatic: IProxyConfigStatic;
  AUserAgentString: string;
  ATimeOut, ASleepOnResetConnection: Cardinal;
  ADownloadTryCount: Integer
);
begin
  FProxyConfigStatic := AProxyConfigStatic;
  FTimeOut := ATimeOut;
  FSleepOnResetConnection := ASleepOnResetConnection;
  FDownloadTryCount := ADownloadTryCount;
  FUserAgentString := AUserAgentString;
end;

function TInetConfigStatic.GetDownloadTryCount: Integer;
begin
  Result := FDownloadTryCount;
end;

function TInetConfigStatic.GetProxyConfigStatic: IProxyConfigStatic;
begin
  Result := FProxyConfigStatic;
end;

function TInetConfigStatic.GetSleepOnResetConnection: Cardinal;
begin
  Result := FSleepOnResetConnection;
end;

function TInetConfigStatic.GetTimeOut: Cardinal;
begin
  Result := FTimeOut;
end;

function TInetConfigStatic.GetUserAgentString: string;
begin
  Result := FUserAgentString;
end;

end.
