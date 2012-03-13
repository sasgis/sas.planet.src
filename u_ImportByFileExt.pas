{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  i_ImportFile,
  i_VectorItmesFactory,
  i_VectorDataFactory,
  i_VectorDataLoader,
  i_ImportConfig;

type
  TImportByFileExt = class(TInterfacedObject, IImportFile)
  private
    FImportXML: IImportFile;
    FImportPLT: IImportFile;
    FImportKML: IImportFile;
    FImportKMZ: IImportFile;
    FImportHLG: IImportFile;
    FImportMP: IImportFile;
  protected
    function ProcessImport(AFileName: string; AConfig: IImportConfig): Boolean;
  public
    constructor Create(
      AVectorDataFactory: IVectorDataFactory;
      AFactory: IVectorItmesFactory;
      AXmlLoader: IVectorDataLoader;
      APltLoader: IVectorDataLoader;
      AKmlLoader: IVectorDataLoader;
      AKmzLoader: IVectorDataLoader
    );
  end;

implementation

uses
  SysUtils,
  u_ImportKML,
  u_ImportHLG,
  u_ImportMpSimple;

{ TImportByFileExt }

constructor TImportByFileExt.Create(
  AVectorDataFactory: IVectorDataFactory;
  AFactory: IVectorItmesFactory;
  AXmlLoader: IVectorDataLoader;
  APltLoader: IVectorDataLoader;
  AKmlLoader: IVectorDataLoader;
  AKmzLoader: IVectorDataLoader
);
begin
  FImportXML := TImportKML.Create(AVectorDataFactory, AXmlLoader);
  FImportPLT := TImportKML.Create(AVectorDataFactory, APltLoader);
  FImportHLG := TImportHLG.Create(AFactory);
  FImportMP := TImportMpSimple.Create(AFactory);
  FImportKML := TImportKML.Create(AVectorDataFactory, AKmlLoader);
  FImportKMZ := TImportKML.Create(AVectorDataFactory, AKmzLoader);
end;

function TImportByFileExt.ProcessImport(AFileName: string; AConfig: IImportConfig): Boolean;
var VExtLwr: String;
begin
  Result := False;
  VExtLwr:=LowerCase(ExtractFileExt(AFileName));
  if ('.gpx'=VExtLwr) then begin
    Result := FImportXML.ProcessImport(AFileName, AConfig);
  end else if ('.kml'=VExtLwr) then begin
    Result := FImportKML.ProcessImport(AFileName, AConfig);
  end else if ('.kmz'=VExtLwr) then begin
    Result := FImportKMZ.ProcessImport(AFileName, AConfig);
  end else if ('.plt'=VExtLwr) then begin
    Result := FImportPLT.ProcessImport(AFileName, AConfig);
  end else if ('.hlg'=VExtLwr) then begin
    Result := FImportHLG.ProcessImport(AFileName, AConfig);
  end else if ('.mp'=VExtLwr) then begin
    Result := FImportMP.ProcessImport(AFileName, AConfig);
  end;
end;

end.
