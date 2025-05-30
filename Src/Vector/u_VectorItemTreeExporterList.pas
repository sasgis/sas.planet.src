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

unit u_VectorItemTreeExporterList;

interface

uses
  i_ExportConfig,
  i_InterfaceListStatic,
  i_VectorItemTreeExporter,
  i_VectorItemTreeExporterList,
  u_BaseInterfacedObject;

type
  TVectorItemTreeExporterListItem = class(TBaseInterfacedObject, IVectorItemTreeExporterListItem)
  private
    FExporter: IVectorItemTreeExporter;
    FConfig: IExportConfig;
    FOptions: TExporterListItemOptions;
    FDefaultExt: string;
    FName: string;
  private
    function GetExporter: IVectorItemTreeExporter;
    function GetConfig: IExportConfig;
    function GetOptions: TExporterListItemOptions;
    function GetDefaultExt: string;
    function GetName: string;
  public
    constructor Create(
      const AExporter: IVectorItemTreeExporter;
      const AConfig: IExportConfig;
      const AOptions: TExporterListItemOptions;
      const ADefaultExt: string;
      const AName: string
    );
  end;

  TVectorItemTreeExporterListStatic = class(TBaseInterfacedObject, IVectorItemTreeExporterListStatic)
  private
    FList: IInterfaceListStatic;
  private
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): IVectorItemTreeExporterListItem;
  public
    constructor Create(const AList: IInterfaceListStatic);
  end;

implementation

{ TVectorItemTreeExporterListItem }

constructor TVectorItemTreeExporterListItem.Create(
  const AExporter: IVectorItemTreeExporter;
  const AConfig: IExportConfig;
  const AOptions: TExporterListItemOptions;
  const ADefaultExt: string;
  const AName: string
);
begin
  Assert(Assigned(AExporter));
  Assert(ADefaultExt <> '');
  Assert(AName <> '');
  inherited Create;
  FExporter := AExporter;
  FConfig := AConfig;
  FOptions := AOptions;
  FDefaultExt := ADefaultExt;
  FName := AName;
end;

function TVectorItemTreeExporterListItem.GetConfig: IExportConfig;
begin
  Result := FConfig;
end;

function TVectorItemTreeExporterListItem.GetOptions: TExporterListItemOptions;
begin
  Result := FOptions;
end;

function TVectorItemTreeExporterListItem.GetDefaultExt: string;
begin
  Result := FDefaultExt;
end;

function TVectorItemTreeExporterListItem.GetExporter: IVectorItemTreeExporter;
begin
  Result := FExporter;
end;

function TVectorItemTreeExporterListItem.GetName: string;
begin
  Result := FName;
end;

{ TVectorItemTreeExporterListStatic }

constructor TVectorItemTreeExporterListStatic.Create(
  const AList: IInterfaceListStatic
);
begin
  inherited Create;
  FList := AList;
end;

function TVectorItemTreeExporterListStatic.GetCount: Integer;
begin
  if Assigned(FList) then begin
    Result := FList.Count;
  end else begin
    Result := 0;
  end;
end;

function TVectorItemTreeExporterListStatic.GetItem(
  const AIndex: Integer
): IVectorItemTreeExporterListItem;
begin
  Result := IVectorItemTreeExporterListItem(FList.Items[AIndex]);
end;

end.
