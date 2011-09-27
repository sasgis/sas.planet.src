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

unit u_ImportKML;

interface

uses
  Classes,
  i_VectorDataLoader,
  i_ImportConfig,
  u_MarksImportBase;

type
  TImportKML = class(TMarksImportBase)
  private
    FKmlLoader: IVectorDataLoader;
  protected
    function DoImport(AFileName: string; AConfig: IImportConfig): IInterfaceList; override;
  public
    constructor Create(AKmlLoader: IVectorDataLoader);
  end;

implementation

uses
  SysUtils,
  i_MarksSimple,
  i_VectorDataItemSimple;

{ TImportKML }

constructor TImportKML.Create(AKmlLoader: IVectorDataLoader);
begin
  FKmlLoader := AKmlLoader;
end;

function TImportKML.DoImport(AFileName: string;
  AConfig: IImportConfig): IInterfaceList;
var
  KML: IVectorDataItemList;
  VMark: IMark;
  VItem: IVectorDataItemSimple;
  i: Integer;
begin
  Result := TInterfaceList.Create;
    FKmlLoader.LoadFromFile(AFileName, KML);
    for i:=0 to KML.Count-1 do begin
      VMark := nil;
      VItem := KML.GetItem(i);
      if VItem.IsPoint then begin
        if AConfig.TemplateNewPoint <> nil then begin
          VMark := AConfig.MarkDB.Factory.CreateNewPoint(
            VItem.Points[0],
            VItem.Name,
            VItem.Desc,
            AConfig.TemplateNewPoint
          );
        end;
      end else if VItem.IsPoly then begin
        if AConfig.TemplateNewPoly <> nil then begin
          VMark := AConfig.MarkDB.Factory.CreateNewPoly(
            VItem.Points,
            VItem.Name,
            VItem.Desc,
            AConfig.TemplateNewPoly
          );
        end;
      end else if VItem.IsLine then begin
        if AConfig.TemplateNewLine <> nil then begin
          VMark := AConfig.MarkDB.Factory.CreateNewLine(
            VItem.Points,
            VItem.Name,
            VItem.Desc,
            AConfig.TemplateNewLine
          );
        end;
      end;
      if VMark <> nil then begin
        Result.Add(VMark);
      end;
    end;
end;

end.
