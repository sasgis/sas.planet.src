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

unit u_ImportConfig;

interface

uses
  i_MarkTemplate,
  i_ImportConfig,
  i_MarksDb;

type
  TImportConfig = class(TInterfacedObject, IImportConfig)
  private
    FMarkDB: IMarksDb;
    FTemplateNewPoint: IMarkTemplatePoint;
    FTemplateNewLine: IMarkTemplateLine;
    FTemplateNewPoly: IMarkTemplatePoly;
  protected
    function GetMarkDB: IMarksDb;
    function GetTemplateNewPoint: IMarkTemplatePoint;
    function GetTemplateNewLine: IMarkTemplateLine;
    function GetTemplateNewPoly: IMarkTemplatePoly;
  public
    constructor Create(
      AMarkDB: IMarksDb;
      ATemplateNewPoint: IMarkTemplatePoint;
      ATemplateNewLine: IMarkTemplateLine;
      ATemplateNewPoly: IMarkTemplatePoly
    );
  end;

implementation

{ TImportConfig }

constructor TImportConfig.Create(
  AMarkDB: IMarksDb;
  ATemplateNewPoint: IMarkTemplatePoint;
  ATemplateNewLine: IMarkTemplateLine;
  ATemplateNewPoly: IMarkTemplatePoly
);
begin
  FMarkDB := AMarkDB;
  FTemplateNewPoint := ATemplateNewPoint;
  FTemplateNewLine := ATemplateNewLine;
  FTemplateNewPoly := ATemplateNewPoly;
end;

function TImportConfig.GetMarkDB: IMarksDb;
begin
  Result := FMarkDB;
end;

function TImportConfig.GetTemplateNewLine: IMarkTemplateLine;
begin
  Result := FTemplateNewLine;
end;

function TImportConfig.GetTemplateNewPoint: IMarkTemplatePoint;
begin
  Result := FTemplateNewPoint;
end;

function TImportConfig.GetTemplateNewPoly: IMarkTemplatePoly;
begin
  Result := FTemplateNewPoly;
end;

end.
