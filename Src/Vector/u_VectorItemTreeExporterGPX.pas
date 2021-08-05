{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit u_VectorItemTreeExporterGPX;

interface

uses
  i_NotifierOperation,
  i_GeoCalc,
  i_BuildInfo,
  i_VectorItemTree,
  i_VectorItemTreeExporter,
  u_BaseInterfacedObject;

type
  TVectorItemTreeExporterGPX = class(TBaseInterfacedObject, IVectorItemTreeExporter)
  private
    FGeoCalc: IGeoCalc;
    FBuildInfo: IBuildInfo;
  private
    procedure ProcessExport(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      const ATree: IVectorItemTree
    );
  public
    constructor Create(const AGeoCalc: IGeoCalc;
                       const ABuildInfo: IBuildInfo);
  end;

implementation

uses
  u_ExportMarks2GPX;

{ TVectorItemTreeExporterGPX }

constructor TVectorItemTreeExporterGPX.Create(
  const AGeoCalc: IGeoCalc;
  const ABuildInfo: IBuildInfo);
begin
  Assert(Assigned(AGeoCalc));
  inherited Create;
  FGeoCalc := AGeoCalc;
  FBuildInfo := ABuildInfo;
end;

procedure TVectorItemTreeExporterGPX.ProcessExport(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const ATree: IVectorItemTree
);
var
  VExport: TExportMarks2GPX;
begin
  VExport := TExportMarks2GPX.Create;
  try
    VExport.ExportTreeToGPX(FGeoCalc, FBuildInfo, ATree, AFileName);
  finally
    VExport.Free
  end;
end;

end.
