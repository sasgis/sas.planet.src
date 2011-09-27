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

unit u_VectorDataItemPoint;

interface

uses
  t_GeoTypes,
  i_HtmlToHintTextConverter,
  u_VectorDataItemBase;

type
  TVectorDataItemPoint = class(TVectorDataItemBase)
  private
    FPoint: TDoublePoint;
  protected
    function IsPoint: Boolean; override;
    function IsLine: Boolean; override;
    function IsPoly: Boolean; override;
    function GetLLRect: TDoubleRect;  override;
    function GetPoints: TArrayOfDoublePoint;  override;
  public
    constructor Create(
      AHintConverter: IHtmlToHintTextConverter;
      AName: string;
      ADesc: string;
      APoint: TDoublePoint
    );
  end;

implementation

{ TVectorDataItemPoint }

constructor TVectorDataItemPoint.Create(
  AHintConverter: IHtmlToHintTextConverter;
  AName, ADesc: string;
  APoint: TDoublePoint
);
begin
  inherited Create(AHintConverter, AName, ADesc);
  FPoint := APoint;
end;

function TVectorDataItemPoint.GetLLRect: TDoubleRect;
begin
  Result.TopLeft := FPoint;
  Result.BottomRight := FPoint;
end;

function TVectorDataItemPoint.GetPoints: TArrayOfDoublePoint;
begin
  SetLength(Result, 1);
  Result[0] := FPoint;
end;

function TVectorDataItemPoint.IsLine: Boolean;
begin
  Result := False;
end;

function TVectorDataItemPoint.IsPoint: Boolean;
begin
  Result := True;
end;

function TVectorDataItemPoint.IsPoly: Boolean;
begin
  Result := False;
end;

end.
