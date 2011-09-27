{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_ImportHLG;

interface

uses
  i_ImportFile,
  i_ImportConfig;

type
  TImportHLG = class(TInterfacedObject, IImportFile)
  protected
    function ProcessImport(AFileName: string; AConfig: IImportConfig): Boolean;
  end;

implementation

uses
  IniFiles,
  SysUtils,
  t_GeoTypes,
  i_MarksSimple,
  u_GeoToStr;

{ TImportHLG }

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
        VPolygon,
        ExtractFileName(AFileName),
        '',
        AConfig.TemplateNewPoly
      );
      if VMark <> nil then begin
        AConfig.MarkDB.WriteMark(VMark);
        Result := True;
      end;
    end;
  end;
end;

end.
