{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_VectorItemTreeExporterTCX;

interface

uses
  i_NotifierOperation,
  i_GeoCalc,
  i_BuildInfo,
  i_VectorItemTree,
  i_VectorItemTreeExporter,
  u_BaseInterfacedObject;

type
  TVectorItemTreeExporterTCX = class(TBaseInterfacedObject, IVectorItemTreeExporter)
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
    constructor Create(const AGeoCalc: IGeoCalc; const ABuildInfo: IBuildInfo);
  end;

implementation

uses
  u_ExportMarks2TCX;

{ TVectorItemTreeExporterTCX }

constructor TVectorItemTreeExporterTCX.Create(const AGeoCalc: IGeoCalc; const ABuildInfo: IBuildInfo);
begin
  Assert(Assigned(AGeoCalc));
  inherited Create;
  FGeoCalc := AGeoCalc;
  FBuildInfo := ABuildInfo;
end;

procedure TVectorItemTreeExporterTCX.ProcessExport(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const ATree: IVectorItemTree
);
var
  VExport: TExportMarks2TCX;
begin
  VExport := TExportMarks2TCX.Create;
  try
    VExport.ExportTreeToTCX(FGeoCalc, FBuildInfo, ATree, AFileName);
  finally
    VExport.Free
  end;
end;

end.
