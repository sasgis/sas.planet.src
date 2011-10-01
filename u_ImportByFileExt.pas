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
  i_VectorDataLoader,
  i_ImportConfig;

type
  TImportByFileExt = class(TInterfacedObject, IImportFile)
  private
    FImportPLT: IImportFile;
    FImportKML: IImportFile;
    FImportKMZ: IImportFile;
    FImportHLG: IImportFile;
    FImportMP: IImportFile;
  protected
    function ProcessImport(AFileName: string; AConfig: IImportConfig): Boolean;
  public
    constructor Create(
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
  APltLoader: IVectorDataLoader;
  AKmlLoader: IVectorDataLoader;
  AKmzLoader: IVectorDataLoader
);
begin
  FImportPLT := TImportKML.Create(APltLoader);
  FImportHLG := TImportHLG.Create;
  FImportMP := TImportMpSimple.Create;
  FImportKML := TImportKML.Create(AKmlLoader);
  FImportKMZ := TImportKML.Create(AKmzLoader);
end;

function TImportByFileExt.ProcessImport(AFileName: string;
  AConfig: IImportConfig): Boolean;
begin
  Result := False;
  if (LowerCase(ExtractFileExt(AFileName))='.kml') then begin
    Result := FImportKML.ProcessImport(AFileName, AConfig);
  end else if (LowerCase(ExtractFileExt(AFileName))='.kmz') then begin
    Result := FImportKMZ.ProcessImport(AFileName, AConfig);
  end else if (LowerCase(ExtractFileExt(AFileName))='.plt') then begin
    Result := FImportPLT.ProcessImport(AFileName, AConfig);
  end else if (LowerCase(ExtractFileExt(AFileName))='.hlg') then begin
    Result := FImportHLG.ProcessImport(AFileName, AConfig);
  end else if (LowerCase(ExtractFileExt(AFileName))='.mp') then begin
    Result := FImportMP.ProcessImport(AFileName, AConfig);
  end;
end;

end.
