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

unit u_ImportHLG;

interface

uses
  i_VectorItmesFactory,
  i_ImportFile,
  i_ImportConfig;

type
  TImportHLG = class(TInterfacedObject, IImportFile)
  private
    FFactory: IVectorItmesFactory;
  private
    function ProcessImport(
      const AFileName: string;
      const AConfig: IImportConfig
    ): Boolean;
  public
    constructor Create(
      const AFactory: IVectorItmesFactory
    );
  end;

implementation

uses
  IniFiles,
  SysUtils,
  i_MarksSimple,
  i_ConfigDataProvider,
  i_VectorItemLonLat,
  u_ConfigDataProviderByIniFile,
  u_ConfigProviderHelpers;

{ TImportHLG }

constructor TImportHLG.Create(const AFactory: IVectorItmesFactory);
begin
  inherited Create;
  FFactory := AFactory;
end;

function TImportHLG.ProcessImport(
  const AFileName: string;
  const AConfig: IImportConfig
): Boolean;
var
  VIni: TMemIniFile;
  VHLGData: IConfigDataProvider;
  VPolygonSection: IConfigDataProvider;
  VPolygon: ILonLatPolygon;
  VMark: IMark;
begin
  Result := False;
  if AConfig.TemplateNewPoly <> nil then begin
    VIni := TMemIniFile.Create(AFileName);
    try
      VHLGData := TConfigDataProviderByIniFile.Create(VIni);
      VIni := nil;
    finally
      FreeAndNil(VIni);
    end;
    VPolygonSection := VHLGData.GetSubItem('HIGHLIGHTING');
    if VPolygonSection <> nil then begin
      VPolygon := ReadPolygon(VPolygonSection, FFactory);
    end;
    if (VPolygon <> nil) and (VPolygon.Count > 0) then begin
      VMark :=
        AConfig.MarkDB.Factory.CreateNewPoly(
          VPolygon,
          ExtractFileName(AFileName),
          '',
          AConfig.TemplateNewPoly
        );
      if VMark <> nil then begin
        AConfig.MarkDB.UpdateMark(nil, VMark);
        Result := True;
      end;
    end;
  end;
end;

end.
