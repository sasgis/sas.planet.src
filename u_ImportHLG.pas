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
  Classes,
  i_VectorItemsFactory,
  i_ImportFile,
  i_ImportConfig,
  u_BaseInterfacedObject;

type
  TImportHLG = class(TBaseInterfacedObject, IImportFile)
  private
    FFactory: IVectorItemsFactory;
  private
    function ProcessImport(
      const AFileName: string;
      const AConfig: IImportConfig
    ): IInterfaceList;
  public
    constructor Create(
      const AFactory: IVectorItemsFactory
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

constructor TImportHLG.Create(const AFactory: IVectorItemsFactory);
begin
  inherited Create;
  FFactory := AFactory;
end;

function TImportHLG.ProcessImport(
  const AFileName: string;
  const AConfig: IImportConfig
): IInterfaceList;
var
  VIniFile: TMemIniFile;
  VHLGData: IConfigDataProvider;
  VPolygonSection: IConfigDataProvider;
  VPolygon: ILonLatPolygon;
  VMark: IMark;
begin
  Result := nil;
  if AConfig.TemplateNewPoly <> nil then begin
    VIniFile := TMemIniFile.Create(AFileName);
    try
      VHLGData := TConfigDataProviderByIniFile.CreateWithOwn(VIniFile);
      VIniFile := nil;
    finally
      FreeAndNil(VIniFile);
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
        VMark := AConfig.MarkDB.UpdateMark(nil, VMark);
        if VMark <> nil then begin
          Result := TInterfaceList.Create;
          Result.Add(VMark);
        end;
      end;
    end;
  end;
end;

end.
