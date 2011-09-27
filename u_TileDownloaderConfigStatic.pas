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

unit u_TileDownloaderConfigStatic;

interface

uses
  i_InetConfig,
  i_TileDownloaderConfig;

type
  TTileDownloaderConfigStatic = class(TInterfacedObject, ITileDownloaderConfigStatic)
  private
    FInetConfigStatic: IInetConfigStatic;
    FWaitInterval: Cardinal;
    FMaxConnectToServerCount: Cardinal;
    FIgnoreMIMEType: Boolean;
    FExpectedMIMETypes: string;
    FDefaultMIMEType: string;
  protected
    function GetInetConfigStatic: IInetConfigStatic;
    function GetWaitInterval: Cardinal;
    function GetMaxConnectToServerCount: Cardinal;
    function GetIgnoreMIMEType: Boolean;
    function GetExpectedMIMETypes: string;
    function GetDefaultMIMEType: string;
  public
    constructor Create(
      AInetConfigStatic: IInetConfigStatic;
      AWaitInterval: Cardinal;
      AMaxConnectToServerCount: Cardinal;
      AIgnoreMIMEType: Boolean;
      AExpectedMIMETypes: string;
      ADefaultMIMEType: string
    );
  end;

implementation

{ TTileDownloaderConfigStatic }

constructor TTileDownloaderConfigStatic.Create(
  AInetConfigStatic: IInetConfigStatic;
  AWaitInterval: Cardinal;
  AMaxConnectToServerCount: Cardinal;
  AIgnoreMIMEType: Boolean;
  AExpectedMIMETypes, ADefaultMIMEType: string
);
begin
  FInetConfigStatic := AInetConfigStatic;
  FWaitInterval := AWaitInterval;
  FMaxConnectToServerCount := AMaxConnectToServerCount;
  FIgnoreMIMEType := AIgnoreMIMEType;
  FExpectedMIMETypes := AExpectedMIMETypes;
  FDefaultMIMEType := ADefaultMIMEType;
end;

function TTileDownloaderConfigStatic.GetDefaultMIMEType: string;
begin
  Result := FDefaultMIMEType;
end;

function TTileDownloaderConfigStatic.GetExpectedMIMETypes: string;
begin
  Result := FExpectedMIMETypes;
end;

function TTileDownloaderConfigStatic.GetIgnoreMIMEType: Boolean;
begin
  Result := FIgnoreMIMEType;
end;

function TTileDownloaderConfigStatic.GetInetConfigStatic: IInetConfigStatic;
begin
  Result := FInetConfigStatic;
end;

function TTileDownloaderConfigStatic.GetMaxConnectToServerCount: Cardinal;
begin
  Result := FMaxConnectToServerCount;
end;

function TTileDownloaderConfigStatic.GetWaitInterval: Cardinal;
begin
  Result := FWaitInterval;
end;

end.
