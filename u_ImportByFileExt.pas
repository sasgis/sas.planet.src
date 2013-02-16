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

unit u_ImportByFileExt;

interface

uses
  Classes,
  i_ImportFile,
  i_VectorItemsFactory,
  i_VectorDataFactory,
  i_VectorDataLoader,
  i_ImportConfig,
  i_MarksSystem,
  u_BaseInterfacedObject;

type
  TImportByFileExt = class(TBaseInterfacedObject, IImportFile)
  private
    FImportGPX: IImportFile;
    FImportPLT: IImportFile;
    FImportCSV: IImportFile;
    FImportKML: IImportFile;
    FImportKMZ: IImportFile;
    FImportHLG: IImportFile;
    FImportMP: IImportFile;
    FImportSLS: IImportFile;
  private
    function ProcessImport(
      const AMarksSystem: IMarksSystem;
      const AFileName: string;
      const AConfig: IImportConfig
    ): IInterfaceList;
  public
    constructor Create(
      const AVectorDataFactory: IVectorDataFactory;
      const AFactory: IVectorItemsFactory;
      const AGpxLoader: IVectorDataLoader;
      const APltLoader: IVectorDataLoader;
      const AKmlLoader: IVectorDataLoader;
      const AKmzLoader: IVectorDataLoader
    );
  end;

implementation

uses
  SysUtils,
  u_ImportKML,
  u_ImportHLG,
  u_ImportCSV,
  u_ImportSLS,
  u_ImportMpSimple;

{ TImportByFileExt }

constructor TImportByFileExt.Create(
  const AVectorDataFactory: IVectorDataFactory;
  const AFactory: IVectorItemsFactory;
  const AGpxLoader: IVectorDataLoader;
  const APltLoader: IVectorDataLoader;
  const AKmlLoader: IVectorDataLoader;
  const AKmzLoader: IVectorDataLoader
);
begin
  inherited Create;
  FImportGPX := TImportByVectorLoader.Create(AVectorDataFactory, AGpxLoader);
  FImportKML := TImportByVectorLoader.Create(AVectorDataFactory, AKmlLoader);
  FImportKMZ := TImportByVectorLoader.Create(AVectorDataFactory, AKmzLoader);
  FImportPLT := TImportByVectorLoader.Create(AVectorDataFactory, APltLoader);
  FImportCSV := TImportCSV.Create(AFactory);
  FImportHLG := TImportHLG.Create(AFactory);
  FImportMP := TImportMpSimple.Create(AFactory);
  FImportSLS := TImportSLS.Create(AFactory);
end;

function TImportByFileExt.ProcessImport(
  const AMarksSystem: IMarksSystem;
  const AFileName: string;
  const AConfig: IImportConfig
): IInterfaceList;
var
  VExtLwr: String;
begin
  Result := nil;
  VExtLwr := LowerCase(ExtractFileExt(AFileName));
  if ('.gpx' = VExtLwr) then begin
    Result := FImportGPX.ProcessImport(AMarksSystem, AFileName, AConfig);
  end else if ('.kml' = VExtLwr) then begin
    Result := FImportKML.ProcessImport(AMarksSystem, AFileName, AConfig);
  end else if ('.kmz' = VExtLwr) then begin
    Result := FImportKMZ.ProcessImport(AMarksSystem, AFileName, AConfig);
  end else if ('.plt' = VExtLwr) then begin
    Result := FImportPLT.ProcessImport(AMarksSystem, AFileName, AConfig);
  end else if ('.csv' = VExtLwr) then begin
    Result := FImportCSV.ProcessImport(AMarksSystem, AFileName, AConfig);
  end else if ('.hlg' = VExtLwr) then begin
    Result := FImportHLG.ProcessImport(AMarksSystem, AFileName, AConfig);
  end else if ('.mp' = VExtLwr) then begin
    Result := FImportMP.ProcessImport(AMarksSystem, AFileName, AConfig);
  end else if ('.sls' = VExtLwr) then begin
    Result := FImportSLS.ProcessImport(AMarksSystem, AFileName, AConfig);
  end;
end;

end.
