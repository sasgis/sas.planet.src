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

unit u_ContentTypeInfo;

interface

uses
  i_BitmapTileSaveLoad,
  i_VectorDataLoader,
  i_ContentTypeInfo;

type
  TContentTypeInfoBase = class(TInterfacedObject, IContentTypeInfoBasic)
  private
    FContentType: WideString;
    FDefaultExt: WideString;
  protected
    function GetContentType: WideString;
    function GetDefaultExt: WideString;
  public
    constructor Create(
      AContentType: WideString;
      ADefaultExt: WideString
    );
  end;

  TContentTypeInfoBitmap = class(TContentTypeInfoBase, IContentTypeInfoBitmap)
  private
    FLoader: IBitmapTileLoader;
    FSaver: IBitmapTileSaver;
  protected
    function GetLoader: IBitmapTileLoader;
    function GetSaver: IBitmapTileSaver;
  public
    constructor Create(
      AContentType: WideString;
      ADefaultExt: WideString;
      ALoader: IBitmapTileLoader;
      ASaver: IBitmapTileSaver
    );
    destructor Destroy; override;
  end;

  TContentTypeInfoKml = class(TContentTypeInfoBase, IContentTypeInfoVectorData)
  private
    FLoader: IVectorDataLoader;
  protected
    function GetLoader: IVectorDataLoader;
  public
    constructor Create(
      AContentType: WideString;
      ADefaultExt: WideString;
      ALoader: IVectorDataLoader
    );
    destructor Destroy; override;
  end;

implementation

{ TContentTypeInfoBase }

constructor TContentTypeInfoBase.Create(AContentType, ADefaultExt: WideString);
begin
  FContentType := AContentType;
  FDefaultExt := ADefaultExt;
end;

function TContentTypeInfoBase.GetContentType: WideString;
begin
  Result := FContentType;
end;

function TContentTypeInfoBase.GetDefaultExt: WideString;
begin
  Result := FDefaultExt;
end;

{ TContentTypeInfoBitmap }

constructor TContentTypeInfoBitmap.Create(AContentType, ADefaultExt: WideString;
  ALoader: IBitmapTileLoader; ASaver: IBitmapTileSaver);
begin
  inherited Create(AContentType, ADefaultExt);
  FLoader := ALoader;
  FSaver := ASaver;
end;

destructor TContentTypeInfoBitmap.Destroy;
begin
  FLoader := nil;
  FSaver := nil;
  inherited;
end;

function TContentTypeInfoBitmap.GetLoader: IBitmapTileLoader;
begin
  Result := FLoader;
end;

function TContentTypeInfoBitmap.GetSaver: IBitmapTileSaver;
begin
  Result := FSaver;
end;

{ TContentTypeInfoKml }

constructor TContentTypeInfoKml.Create(AContentType, ADefaultExt: WideString;
  ALoader: IVectorDataLoader);
begin
  inherited Create(AContentType, ADefaultExt);
  FLoader := ALoader;
end;

destructor TContentTypeInfoKml.Destroy;
begin
  FLoader := nil;
  inherited;
end;

function TContentTypeInfoKml.GetLoader: IVectorDataLoader;
begin
  Result := FLoader;
end;

end.
