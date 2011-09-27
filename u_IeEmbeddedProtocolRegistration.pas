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

unit u_IeEmbeddedProtocolRegistration;

interface

uses
  UrlMon,
  ActiveX;

type
  TIeEmbeddedProtocolRegistration = class
  private
    FProtocol: WideString;
    FFactory : IClassFactory;
    FInternetSession: IInternetSession;
  public
    constructor Create(
      AProtocol: PWideChar;
      AFactory : IClassFactory
    );
    destructor Destroy; override;
  end;

implementation

const
  CIEEmbeddedProtocol_Class: TGUID = '{A9CA884C-253A-4662-A4F6-6926BAB877F9}';

{ TIeEmbeddedProtocolRegistration }

constructor TIeEmbeddedProtocolRegistration.Create(AProtocol: PWideChar;
  AFactory: IClassFactory);
begin
  FProtocol := AProtocol;
  FFactory := AFactory;
  CoInternetGetSession(0, FInternetSession, 0);
  FInternetSession.RegisterNameSpace(FFactory, CIEEmbeddedProtocol_Class, PWideChar(FProtocol), 0, nil, 0);
end;

destructor TIeEmbeddedProtocolRegistration.Destroy;
begin
  FInternetSession.UnregisterNameSpace(FFactory, PWideChar(FProtocol));
  FFactory := nil;
  FInternetSession := nil;
  inherited;
end;

end.
