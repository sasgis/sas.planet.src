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
  protected
    function ProcessImport(AFileName: string; AConfig: IImportConfig): Boolean;
  public
    constructor Create(
      AFactory: IVectorItmesFactory
    );
  end;

implementation

uses
  IniFiles,
  SysUtils,
  t_GeoTypes,
  i_MarksSimple,
  u_GeoToStr;

{ TImportHLG }

constructor TImportHLG.Create(AFactory: IVectorItmesFactory);
begin
  FFactory := AFactory;
end;

function TImportHLG.ProcessImport(AFileName: string;
  AConfig: IImportConfig): Boolean;
var
  ini:TMemIniFile;
  i:integer;
  VPolygon: TArrayOfDoublePoint;
  VMark: IMark;
begin
  Result := False;
  VPolygon := nil;
  if AConfig.TemplateNewPoly <> nil then begin
    ini:=TMemIniFile.Create(AFileName);
    try
      i:=1;
      while str2r(Ini.ReadString('HIGHLIGHTING','PointLon_'+IntToStr(i),'2147483647'))<>2147483647 do begin
        setlength(VPolygon,i);
        VPolygon[i-1].x:=str2r(Ini.ReadString('HIGHLIGHTING','PointLon_'+IntToStr(i),'2147483647'));
        VPolygon[i-1].y:=str2r(Ini.ReadString('HIGHLIGHTING','PointLat_'+IntToStr(i),'2147483647'));
        inc(i);
      end;
    finally
      FreeAndNil(ini);
    end;
    if Length(VPolygon) > 2 then begin
      VMark := AConfig.MarkDB.Factory.CreateNewPoly(
        FFactory.CreateLonLatPolygon(@VPolygon[0], Length(VPolygon)),
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
