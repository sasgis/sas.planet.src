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

unit u_DownloadResultTextProvider;

interface

uses
  i_JclNotify,
  i_LanguageManager,
  i_DownloadResultTextProvider;

type
  TDownloadResultTextProvider = class(TInterfacedObject, IDownloadResultTextProvider)
  private
    FLangManager: ILanguageManager;
    FLangListener: IJclListener;

    FMessageBadContentType: string;
    FMessageBadProxyAuth: string;
    FMessageBanned: string;
    FMessageDataNotExistsByStatusCode: string;
    FMessageDataNotExistsZeroSize: string;
    FMessageLoadErrorByErrorCode: string;
    FMessageLoadErrorByStatusCode: string;
    FMessageLoadErrorByUnknownStatusCode: string;
    FMessageNoConnetctToServerByErrorCode: string;
    FMessageUnexpectedProxyAuth: string;

    procedure OnLangChange(Sender: TObject);
  protected
    function GetMessageBadContentType: string;
    function GetMessageBadProxyAuth: string;
    function GetMessageBanned: string;
    function GetMessageDataNotExistsByStatusCode: string;
    function GetMessageDataNotExistsZeroSize: string;
    function GetMessageLoadErrorByErrorCode: string;
    function GetMessageLoadErrorByStatusCode: string;
    function GetMessageLoadErrorByUnknownStatusCode: string;
    function GetMessageNoConnetctToServerByErrorCode: string;
    function GetMessageUnexpectedProxyAuth: string;
  public
    constructor Create(
      ALangManager: ILanguageManager
    );
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  u_NotifyEventListener;

{ TDownloadResultTextProvider }

constructor TDownloadResultTextProvider.Create(ALangManager: ILanguageManager);
begin
  FLangManager := ALangManager;
  FLangListener := TNotifyEventListener.Create(Self.OnLangChange);
  FLangManager.GetChangeNotifier.Add(FLangListener);

  OnLangChange(nil);
end;

destructor TDownloadResultTextProvider.Destroy;
begin
  FLangManager.GetChangeNotifier.Remove(FLangListener);
  FLangManager := nil;
  FLangListener := nil;
  inherited;
end;

function TDownloadResultTextProvider.GetMessageBadContentType: string;
begin
  Result := FMessageBadContentType;
end;

function TDownloadResultTextProvider.GetMessageBadProxyAuth: string;
begin
  Result := FMessageBadProxyAuth
end;

function TDownloadResultTextProvider.GetMessageBanned: string;
begin
  Result := FMessageBanned;
end;

function TDownloadResultTextProvider.GetMessageDataNotExistsByStatusCode: string;
begin
  Result := FMessageDataNotExistsByStatusCode;
end;

function TDownloadResultTextProvider.GetMessageDataNotExistsZeroSize: string;
begin
  Result := FMessageDataNotExistsZeroSize;
end;

function TDownloadResultTextProvider.GetMessageLoadErrorByErrorCode: string;
begin
  Result := FMessageLoadErrorByErrorCode;
end;

function TDownloadResultTextProvider.GetMessageLoadErrorByStatusCode: string;
begin
  Result := FMessageLoadErrorByStatusCode;
end;

function TDownloadResultTextProvider.GetMessageLoadErrorByUnknownStatusCode: string;
begin
  Result := FMessageLoadErrorByUnknownStatusCode;
end;

function TDownloadResultTextProvider.GetMessageNoConnetctToServerByErrorCode: string;
begin
  Result := FMessageNoConnetctToServerByErrorCode;
end;

function TDownloadResultTextProvider.GetMessageUnexpectedProxyAuth: string;
begin
  Result := FMessageUnexpectedProxyAuth;
end;

procedure TDownloadResultTextProvider.OnLangChange(Sender: TObject);
begin
  FMessageBadContentType := _('Server returned unexpected type "%0:s"');
  FMessageBadProxyAuth := _('Proxy authorization error');
  FMessageBanned := _('Most likely you''ve been banned by the server!');
  FMessageDataNotExistsByStatusCode := _('Tile is not found! Status code %d');
  FMessageDataNotExistsZeroSize := _('Tile is not found! Zero result size');
  FMessageLoadErrorByErrorCode := _('Download error. Error code %d');
  FMessageLoadErrorByStatusCode := _('Download error. Status code %d');
  FMessageLoadErrorByUnknownStatusCode := _('Download error. Unknown status code %d');
  FMessageNoConnetctToServerByErrorCode := _('Connct to server error. Error code %d');
  FMessageUnexpectedProxyAuth := _('Unexpected proxy authorization');
end;

end.
