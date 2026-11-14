{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_VectorItemTreeImporterSHP;

interface

uses
  i_NotifierOperation,
  i_GeometryLonLatFactory,
  i_VectorDataItemSimple,
  i_VectorDataFactory,
  i_VectorItemSubsetBuilder,
  i_ProjConverter,
  i_VectorItemTree,
  i_VectorItemTreeImporter,
  u_BaseInterfacedObject;

type
  TVectorItemTreeImporterSHP = class(TBaseInterfacedObject, IVectorItemTreeImporter)
  private
    FVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
    FVectorDataFactory: IVectorDataFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FProjConverterFactory: IProjConverterFactory;
  private
    function ProcessImport(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      const AConfig: IInterface
    ): IVectorItemTree;
  public
    constructor Create(
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AProjConverterFactory: IProjConverterFactory
    );
  end;

implementation

uses
  u_ImportMarksFromSHP;

{ TVectorItemTreeImporterSHP }

constructor TVectorItemTreeImporterSHP.Create(
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AProjConverterFactory: IProjConverterFactory
);
begin
  inherited Create;
  FVectorDataItemMainInfoFactory := AVectorDataItemMainInfoFactory;
  FVectorDataFactory := AVectorDataFactory;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FProjConverterFactory := AProjConverterFactory;
end;

function TVectorItemTreeImporterSHP.ProcessImport(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AConfig: IInterface
): IVectorItemTree;
var
  VImport: TImportMarksFromSHP;
begin
  VImport :=
    TImportMarksFromSHP.Create(
      FVectorDataItemMainInfoFactory,
      FVectorDataFactory,
      FVectorGeometryLonLatFactory,
      FVectorItemSubsetBuilderFactory,
      FProjConverterFactory
    );
  try
    Result := VImport.ImportFileToTree(AOperationID, ACancelNotifier, AFileName);
  finally
    VImport.Free;
  end;
end;

end.
