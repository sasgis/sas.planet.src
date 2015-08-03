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

unit u_VectorItemTreeExporterGPX;

interface

uses
  i_NotifierOperation,
  i_VectorItemTree,
  i_VectorItemTreeExporter,
  i_ArchiveReadWriteFactory,
  u_BaseInterfacedObject;

type
  TVectorItemTreeExporterGPX = class(TBaseInterfacedObject, IVectorItemTreeExporter)
  private
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
  private
    procedure ProcessExport(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      const ATree: IVectorItemTree
    );
  public
    constructor Create(const AArchiveReadWriteFactory: IArchiveReadWriteFactory);
  end;

implementation

uses
  u_GlobalState,
  u_ExportMarks2GPX;

{ TVectorItemTreeExporterGPX }

constructor TVectorItemTreeExporterGPX.Create(
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory
);
begin
  inherited Create;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
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
  VExport := TExportMarks2GPX.Create(FArchiveReadWriteFactory);
  try
    VExport.ExportTreeToGPX(GState.GeoCalc, ATree, AFileName);
  finally
    VExport.Free
  end;
end;

end.
