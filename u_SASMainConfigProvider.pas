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

unit u_SASMainConfigProvider;

interface

uses
  IniFiles,
  u_ConfigDataWriteProviderWithGlobal;

type
  TSASMainConfigProvider = class(TConfigDataWriteProviderWithGlobal)
  private
    FMainIni: TMemIniFile;
    function GetMainConfigFileName(ABasePath, AExeFileName: string): string;
  public
    constructor Create(ABasePath, AExeFileName: string; AHandle: THandle);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataWriteProviderByIniFile,
  u_ConfigDataProviderVirtualWithSubItem,
  u_ConfigDataProviderByResources;

{ TSASMainConfigProvider }

constructor TSASMainConfigProvider.Create(ABasePath, AExeFileName: string; AHandle: THandle);
var
  VResourceProvider: IConfigDataProvider;
  VGlobalProvider: IConfigDataProvider;
  VMainProvider: IConfigDataWriteProvider;
begin
  VResourceProvider := TConfigDataProviderByResources.Create(AHandle);
  VGlobalProvider := TConfigDataProviderVirtualWithSubItem.Create('Resource', VResourceProvider);
  FMainIni := TMeminifile.Create(GetMainConfigFileName(ABasePath, AExeFileName));
  VMainProvider := TConfigDataWriteProviderByIniFile.Create(FMainIni);
  inherited Create(VMainProvider, 'sas:\', VGlobalProvider);
end;

destructor TSASMainConfigProvider.Destroy;
begin
  try
    FMainIni.UpdateFile;
  except
  end;
  inherited;
end;

function TSASMainConfigProvider.GetMainConfigFileName(ABasePath, AExeFileName: string): string;
var
  VPos: Integer;
begin
  VPos := Pos('.', AExeFileName);
  Result := ABasePath + LeftStr(AExeFileName, VPos - 1) + '.ini';
end;

end.
