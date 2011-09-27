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

unit u_InternalDomainInfoProviderByMapTypeList;

interface

uses
  Classes,
  i_ConfigDataProvider,
  i_ZmpInfo,
  i_ZmpInfoSet,
  i_ContentTypeManager,
  i_InternalDomainInfoProvider;

type
  TInternalDomainInfoProviderByMapTypeList = class(TInterfacedObject, IInternalDomainInfoProvider)
  private
    FZmpInfoSet: IZmpInfoSet;
    FContentTypeManager: IContentTypeManager;
    function ParseFilePath(AFilePath: string; out AZmpGUID: TGUID; out AFileName: string): Boolean;
    function LoadStreamFromZmp(AZmp: IZmpInfo; AFileName: string; AStream: TStream; out AContentType: string): Boolean;
    function LoadStreamFromDataProvider(ADataProvider: IConfigDataProvider; AFileName: string; AStream: TStream; out AContentType: string): Boolean;
    function LoadStreamFromSubDataProvider(ADataProvider: IConfigDataProvider; AFileName: string; AStream: TStream; out AContentType: string): Boolean;
  protected
    function LoadStreamByFilePath(AFilePath: string; AStream: TStream; out AContentType: string): Boolean;
  public
    constructor Create(
      AZmpInfoSet: IZmpInfoSet;
      AContentTypeManager: IContentTypeManager
    );
  end;

implementation

uses
  StrUtils,
  SysUtils,
  i_ContentTypeInfo,
  c_ZeroGUID;

const
  CFileNameSeparator = '/';

{ TInternalDomainInfoProviderByMapTypeList }

constructor TInternalDomainInfoProviderByMapTypeList.Create(
  AZmpInfoSet: IZmpInfoSet;
  AContentTypeManager: IContentTypeManager
);
begin
  FZmpInfoSet := AZmpInfoSet;
  FContentTypeManager := AContentTypeManager;
end;

function TInternalDomainInfoProviderByMapTypeList.LoadStreamByFilePath(
  AFilePath: string; AStream: TStream; out AContentType: string): Boolean;
var
  VGuid: TGUID;
  VZmp: IZmpInfo;
  VFileName: string;
begin
  Result := ParseFilePath(AFilePath, VGuid, VFileName);
  if Result then begin
    Result := False;
    VZmp := FZmpInfoSet.GetZmpByGUID(VGuid);
    if VZmp <> nil then begin
      Result := LoadStreamFromZmp(VZmp, VFileName, AStream, AContentType);
    end;
  end;
end;

function TInternalDomainInfoProviderByMapTypeList.LoadStreamFromDataProvider(
  ADataProvider: IConfigDataProvider; AFileName: string; AStream: TStream;
  out AContentType: string): Boolean;
var
  VFileName: string;
  VExt: string;
  VContentType: IContentTypeInfoBasic;
begin
  AContentType := '';
  VFileName := AFileName;
  if VFileName = '' then begin
    VFileName := 'index.html';
  end;
  if AContentType = '' then begin
    VExt := ExtractFileExt(VFileName);
    VContentType := FContentTypeManager.GetInfoByExt(VExt);
    if VContentType <> nil then begin
      AContentType := VContentType.GetContentType;
    end else begin
      AContentType := 'text/html'
    end;
  end;

  Result := ADataProvider.ReadBinaryStream(VFileName, AStream) > 0;
end;

function TInternalDomainInfoProviderByMapTypeList.LoadStreamFromSubDataProvider(
  ADataProvider: IConfigDataProvider; AFileName: string; AStream: TStream;
  out AContentType: string): Boolean;
var
  VSubItemName: string;
  VFileName: string;
  VPos: Integer;
  VSubItemProvider: IConfigDataProvider;
begin
  VSubItemName := '';
  VFileName := '';
  VPos := Pos(CFileNameSeparator, AFileName);
  if VPos > 0 then begin
    VSubItemName := LeftStr(AFileName, VPos - 1);
    VFileName := RightStr(AFileName, Length(AFileName) - VPos - Length(CFileNameSeparator) + 1);
    if VSubItemName <> '' then begin
      VSubItemProvider := ADataProvider.GetSubItem(VSubItemName);
    end else begin
      VSubItemProvider := ADataProvider;
    end;
    if VSubItemProvider <> nil then begin
      Result := LoadStreamFromSubDataProvider(VSubItemProvider, VFileName, AStream, AContentType);
    end else begin
      Result := False;
    end;
  end else begin
    VFileName := AFileName;
    Result := LoadStreamFromDataProvider(ADataProvider, VFileName, AStream, AContentType);
  end;
end;

function TInternalDomainInfoProviderByMapTypeList.LoadStreamFromZmp(
  AZmp: IZmpInfo; AFileName: string; AStream: TStream; out AContentType: string): Boolean;
begin
  Result := LoadStreamFromSubDataProvider(AZmp.DataProvider, AFileName, AStream, AContentType);
end;

function TInternalDomainInfoProviderByMapTypeList.ParseFilePath(
  AFilePath: string; out AZmpGUID: TGUID; out AFileName: string): Boolean;
var
  VGUIDString: string;
  VPos: Integer;
begin
  AZmpGUID := CGUID_Zero;
  AFileName := '';

  VPos := Pos(CFileNameSeparator, AFilePath);
  if VPos > 0 then begin
    VGUIDString := LeftStr(AFilePath, VPos - 1);
    AFileName := RightStr(AFilePath, Length(AFilePath) - VPos - Length(CFileNameSeparator) + 1);
  end else begin
    VGUIDString := AFilePath;
  end;
  if Length(VGUIDString) > 0 then begin
    try
      AZmpGUID := StringToGUID(VGUIDString);
    except
      AZmpGUID := CGUID_Zero;
    end;
  end;
  Result := not IsEqualGUID(AZmpGUID, CGUID_Zero)
end;

end.
