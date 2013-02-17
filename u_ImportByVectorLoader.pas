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

unit u_ImportByVectorLoader;

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
  TImportByVectorLoader = class(TBaseInterfacedObject, IImportFile)
  private
    FVectorDataFactory: IVectorDataFactory;
    FLoader: IVectorDataLoader;
  private
    function ProcessImport(
      const AMarksSystem: IMarksSystem;
      const AFileName: string;
      const AConfig: IImportConfig
    ): IInterfaceList;
  public
    constructor Create(
      const AVectorDataFactory: IVectorDataFactory;
      const ALoader: IVectorDataLoader
    );
  end;

implementation

uses
  SysUtils,
  i_MarksSimple,
  i_BinaryData,
  i_VectorDataItemSimple,
  u_BinaryDataByMemStream;

{ TImportByVectorLoader }

constructor TImportByVectorLoader.Create(
  const AVectorDataFactory: IVectorDataFactory;
  const ALoader: IVectorDataLoader
);
begin
  inherited Create;
  FVectorDataFactory := AVectorDataFactory;
  FLoader := ALoader;
end;

function TImportByVectorLoader.ProcessImport(
  const AMarksSystem: IMarksSystem;
  const AFileName: string;
  const AConfig: IImportConfig
): IInterfaceList;
var
  VMemStream: TMemoryStream;
  VData: IBinaryData;
  VVectorData: IVectorDataItemList;
begin
  Result := nil;
  VMemStream := TMemoryStream.Create;
  try
    VMemStream.LoadFromFile(AFileName);
    VData := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
    VMemStream := nil;
    VVectorData := FLoader.Load(VData, nil, FVectorDataFactory);
    if Assigned(VVectorData) then begin
      Result := AMarksSystem.ImportItemsList(VVectorData, AConfig);
    end;
  finally
    VMemStream.Free;
  end;
end;

end.
