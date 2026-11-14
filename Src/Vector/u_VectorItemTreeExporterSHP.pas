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

unit u_VectorItemTreeExporterSHP;

interface

uses
  i_NotifierOperation,
  i_VectorItemTree,
  i_VectorItemTreeExporter,
  u_BaseInterfacedObject;

type
  TVectorItemTreeExporterSHP = class(TBaseInterfacedObject, IVectorItemTreeExporter)
  private
    { IVectorItemTreeExporter }
    procedure ProcessExport(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      const ATree: IVectorItemTree
    );
  end;

implementation

uses
  u_ExportMarks2SHP;

{ TVectorItemTreeExporterSHP }

procedure TVectorItemTreeExporterSHP.ProcessExport(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const ATree: IVectorItemTree
);
var
  VExport: TExportMarks2SHP;
begin
  VExport := TExportMarks2SHP.Create;
  try
    VExport.ExportTreeToSHP(AOperationID, ACancelNotifier, ATree, AFileName);
  finally
    VExport.Free;
  end;
end;

end.
