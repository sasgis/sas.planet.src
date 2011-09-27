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
