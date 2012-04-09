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
  i_VectorDataFactory,
  i_ImportConfig,
  u_MarksImportBase;

type
  TImportKML = class(TMarksImportBase)
  private
    FKmlLoader: IVectorDataLoader;
  protected
    function DoImport(
      const AFileName: string;
      const AConfig: IImportConfig
    ): IInterfaceList; override;
  public
    constructor Create(
      const AVectorDataFactory: IVectorDataFactory;
      const AKmlLoader: IVectorDataLoader
    );
  end;

implementation

uses
  SysUtils,
  i_MarksSimple,
  i_VectorDataItemSimple;

{ TImportKML }

constructor TImportKML.Create(
  const AVectorDataFactory: IVectorDataFactory;
  const AKmlLoader: IVectorDataLoader
);
begin
  inherited Create(AVectorDataFactory);
  FKmlLoader := AKmlLoader;
end;

function TImportKML.DoImport(
  const AFileName: string;
  const AConfig: IImportConfig
): IInterfaceList;
var
  KML: IVectorDataItemList;
  VMark: IMark;
  VItem: IVectorDataItemSimple;
  VItemPoint: IVectorDataItemPoint;
  VItemLine: IVectorDataItemLine;
  VItemPoly: IVectorDataItemPoly;
  i: Integer;
  VStream: TFileStream;
begin
  Result := TInterfaceList.Create;
  VStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    KML := FKmlLoader.LoadFromStream(VStream, VectorDataFactory);
    if Assigned(KML) then
    if (0<KML.Count) then
    for i:=0 to KML.Count-1 do begin
      VMark := nil;
      VItem := KML.GetItem(i);
      if Supports(VItem, IVectorDataItemPoint, VItemPoint) then begin
        if AConfig.TemplateNewPoint <> nil then begin
          VMark := AConfig.MarkDB.Factory.CreateNewPoint(
            VItemPoint.Point,
            VItemPoint.Name,
            VItemPoint.Desc,
            AConfig.TemplateNewPoint
          );
        end;
      end else if Supports(VItem, IVectorDataItemPoly, VItemPoly) then begin
        if AConfig.TemplateNewPoly <> nil then begin
          VMark := AConfig.MarkDB.Factory.CreateNewPoly(
            VItemPoly.Line,
            VItemPoly.Name,
            VItemPoly.Desc,
            AConfig.TemplateNewPoly
          );
        end;
      end else if Supports(VItem, IVectorDataItemLine, VItemLine) then begin
        if AConfig.TemplateNewLine <> nil then begin
          VMark := AConfig.MarkDB.Factory.CreateNewLine(
            VItemLine.Line,
            VItemLine.Name,
            VItemLine.Desc,
            AConfig.TemplateNewLine
          );
        end;
      end;
      if VMark <> nil then begin
        Result.Add(VMark);
      end;
    end;
  finally
    VStream.Free;
  end;
end;

end.
