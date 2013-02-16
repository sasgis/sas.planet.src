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

unit u_ImportKML;

interface

uses
  Classes,
  i_VectorDataLoader,
  i_VectorDataFactory,
  i_ImportConfig,
  i_ImportFile,
  i_MarksSystem,
  u_BaseInterfacedObject;

type
  TImportKML = class(TBaseInterfacedObject, IImportFile)
  private
    FVectorDataFactory: IVectorDataFactory;
    FKmlLoader: IVectorDataLoader;
  private
    function ProcessImport(
      const AMarksSystem: IMarksSystem;
      const AFileName: string;
      const AConfig: IImportConfig
    ): IInterfaceList;
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
  inherited Create;
  FVectorDataFactory := AVectorDataFactory;
  FKmlLoader := AKmlLoader;
end;

function TImportKML.ProcessImport(
  const AMarksSystem: IMarksSystem;
  const AFileName: string;
  const AConfig: IImportConfig
): IInterfaceList;
var
  KML: IVectorDataItemList;
  VStream: TFileStream;
begin
  Result := nil;
  VStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    KML := FKmlLoader.LoadFromStream(VStream, nil, FVectorDataFactory);
    if Assigned(KML) then begin
      Result := AMarksSystem.ImportItemsList(KML, AConfig);
    end;
  finally
    VStream.Free;
  end;
end;

end.
