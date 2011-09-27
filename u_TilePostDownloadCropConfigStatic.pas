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

unit u_TilePostDownloadCropConfigStatic;

interface

uses
  Types,
  i_TilePostDownloadCropConfig;

type
  TTilePostDownloadCropConfigStatic = class(TInterfacedObject, ITilePostDownloadCropConfigStatic)
  private
    FIsCropOnDownload: Boolean;
    FCropRect: TRect;
  protected
    function GetIsCropOnDownload: Boolean;
    function GetCropRect: TRect;
  public
    constructor Create(
      ACropRect: TRect
    );
  end;

implementation

{ TTilePostDownloadCropConfigStatic }

constructor TTilePostDownloadCropConfigStatic.Create(ACropRect: TRect);
begin
  if
    (ACropRect.Left >= 0) and
    (ACropRect.Top >= 0) and
    (ACropRect.Right > ACropRect.Left) and
    (ACropRect.Bottom > ACropRect.Top) and
    (ACropRect.Right < 10000) and
    (ACropRect.Bottom < 10000)
  then begin
    FIsCropOnDownload := True;
    FCropRect := ACropRect;
  end else begin
    FIsCropOnDownload := False;
    FCropRect := Rect(0, 0, 0, 0);
  end;
end;

function TTilePostDownloadCropConfigStatic.GetCropRect: TRect;
begin
  Result := FCropRect;
end;

function TTilePostDownloadCropConfigStatic.GetIsCropOnDownload: Boolean;
begin
  Result := FIsCropOnDownload;
end;

end.
