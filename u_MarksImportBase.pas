{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_MarksImportBase;

interface

uses
  Classes,
  i_VectorDataFactory,
  i_ImportFile,
  i_ImportConfig,
  u_BaseInterfacedObject;

type
  TMarksImportBase = class(TBaseInterfacedObject, IImportFile)
  private
    FVectorDataFactory: IVectorDataFactory;
  protected
    property VectorDataFactory: IVectorDataFactory read FVectorDataFactory;
    function DoImport(
      const AFileName: string;
      const AConfig: IImportConfig
    ): IInterfaceList; virtual; abstract;
  private
    function ProcessImport(
      const AFileName: string;
      const AConfig: IImportConfig
    ): IInterfaceList;
  public
    constructor Create(
      const AVectorDataFactory: IVectorDataFactory
    );
  end;

implementation

{ TMarksImportBase }

constructor TMarksImportBase.Create(const AVectorDataFactory: IVectorDataFactory);
begin
  inherited Create;
  FVectorDataFactory := AVectorDataFactory;
end;

function TMarksImportBase.ProcessImport(
  const AFileName: string;
  const AConfig: IImportConfig
): IInterfaceList;
var
  VList: IInterfaceList;
begin
  Result := nil;
  VList := DoImport(AFileName, AConfig);
  if VList <> nil then begin
    if VList.Count > 0 then begin
      Result := AConfig.MarkDB.UpdateMarksList(nil, VList);
    end;
  end;
end;

end.
