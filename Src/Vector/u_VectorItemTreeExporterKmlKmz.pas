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

unit u_VectorItemTreeExporterKmlKmz;

interface

uses
  i_NotifierOperation,
  i_VectorItemTree,
  i_VectorItemTreeExporter,
  i_ArchiveReadWriteFactory,
  u_BaseInterfacedObject;

type
  TVectorItemTreeExporterKmlKmz = class(TBaseInterfacedObject, IVectorItemTreeExporter)
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
  u_ExportMarks2KML;

{ TVectorItemTreeExporterKmlKmz }

constructor TVectorItemTreeExporterKmlKmz.Create(
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory
);
begin
  inherited Create;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

procedure TVectorItemTreeExporterKmlKmz.ProcessExport(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const ATree: IVectorItemTree
);
var
  VExport: TExportMarks2KML;
begin
  VExport := TExportMarks2KML.Create(FArchiveReadWriteFactory);
  try
    VExport.ExportTreeToKML(ATree, AFileName);
  finally
    VExport.Free
  end;
end;

end.
