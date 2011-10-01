{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
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
