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

unit u_ContentConverterBase;

interface

uses
  Classes,
  i_ContentTypeInfo,
  i_ContentConverter;

type

  TContentConverterAbstract = class(TInterfacedObject, IContentConverter)
  private
    FSource: IContentTypeInfoBasic;
    FTarget: IContentTypeInfoBasic;
  protected
    function GetSource: IContentTypeInfoBasic;
    function GetTarget: IContentTypeInfoBasic;
    function GetIsSimpleCopy: Boolean; virtual; abstract;
    procedure ConvertStream(ASource, ATarget: TStream); virtual; abstract;
  public
    constructor Create(
      ASource: IContentTypeInfoBasic;
      ATarget: IContentTypeInfoBasic
    );
    destructor Destroy; override;
  end;

  TContentConverterBase = class(TContentConverterAbstract)
  protected
    function GetIsSimpleCopy: Boolean; override;
  end;

  TContentConverterSimpleCopy = class(TContentConverterAbstract)
  protected
    function GetIsSimpleCopy: Boolean; override;
    procedure ConvertStream(ASource, ATarget: TStream); override;
  end;

implementation

{ TContentConverterAbstract }

constructor TContentConverterAbstract.Create(ASource,
  ATarget: IContentTypeInfoBasic);
begin
  FSource := ASource;
  FTarget := ATarget;
end;

destructor TContentConverterAbstract.Destroy;
begin
  FSource := nil;
  FTarget := nil;
  inherited;
end;

function TContentConverterAbstract.GetSource: IContentTypeInfoBasic;
begin
  Result := FSource;
end;

function TContentConverterAbstract.GetTarget: IContentTypeInfoBasic;
begin
  Result := FTarget;
end;

{ TContentConverterBase }

function TContentConverterBase.GetIsSimpleCopy: Boolean;
begin
  Result := False;
end;

{ TContentConverterSimpleCopy }

procedure TContentConverterSimpleCopy.ConvertStream(ASource, ATarget: TStream);
begin
  ATarget.CopyFrom(ASource, ASource.Size);
end;

function TContentConverterSimpleCopy.GetIsSimpleCopy: Boolean;
begin
  Result := True;
end;

end.
