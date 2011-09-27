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

unit u_SimpleTileStorageConfigStatic;

interface

uses
  i_CoordConverter,
  i_SimpleTileStorageConfig;

type
  TSimpleTileStorageConfigStatic = class(TInterfacedObject, ISimpleTileStorageConfigStatic)
  private
    FCoordConverter: ICoordConverter;
    FCacheTypeCode: Integer;
    FNameInCache: string;
    FTileFileExt: string;
    FIsStoreFileCache: Boolean;
    FIsReadOnly: boolean;
    FAllowDelete: boolean;
    FAllowAdd: boolean;
    FAllowReplace: boolean;
  protected
    function GetCoordConverter: ICoordConverter;
    function GetCacheTypeCode: Integer;
    function GetNameInCache: string;
    function GetTileFileExt: string;
    function GetIsStoreFileCache: Boolean;
    function GetIsReadOnly: boolean;
    function GetAllowDelete: boolean;
    function GetAllowAdd: boolean;
    function GetAllowReplace: boolean;
  public
    constructor Create(
      ACoordConverter: ICoordConverter;
      ACacheTypeCode: Integer;
      ANameInCache: string;
      ATileFileExt: string;
      AIsStoreFileCache: Boolean;
      AIsReadOnly: boolean;
      AAllowDelete: boolean;
      AAllowAdd: boolean;
      AAllowReplace: boolean
    );
  end;

implementation

{ TSimpleTileStorageConfigStatic }

constructor TSimpleTileStorageConfigStatic.Create(
  ACoordConverter: ICoordConverter;
  ACacheTypeCode: Integer;
  ANameInCache: string;
  ATileFileExt: string;
  AIsStoreFileCache, AIsReadOnly, AAllowDelete, AAllowAdd, AAllowReplace: boolean
);
begin
  FCoordConverter := ACoordConverter;
  FCacheTypeCode := ACacheTypeCode;
  FNameInCache := ANameInCache;
  FTileFileExt := ATileFileExt;
  FIsStoreFileCache := AIsStoreFileCache;
  FIsReadOnly := AIsReadOnly;
  FAllowDelete := AAllowDelete;
  FAllowAdd := AAllowAdd;
  FAllowReplace := AAllowReplace;
end;

function TSimpleTileStorageConfigStatic.GetAllowAdd: boolean;
begin
  Result := FAllowAdd;
end;

function TSimpleTileStorageConfigStatic.GetAllowDelete: boolean;
begin
  Result := FAllowDelete;
end;

function TSimpleTileStorageConfigStatic.GetAllowReplace: boolean;
begin
  Result := FAllowReplace;
end;

function TSimpleTileStorageConfigStatic.GetCacheTypeCode: Integer;
begin
  Result := FCacheTypeCode;
end;

function TSimpleTileStorageConfigStatic.GetCoordConverter: ICoordConverter;
begin
  Result := FCoordConverter;
end;

function TSimpleTileStorageConfigStatic.GetIsReadOnly: boolean;
begin
  Result := FIsReadOnly;
end;

function TSimpleTileStorageConfigStatic.GetIsStoreFileCache: Boolean;
begin
  Result := FIsStoreFileCache;
end;

function TSimpleTileStorageConfigStatic.GetNameInCache: string;
begin
  Result := FNameInCache;
end;

function TSimpleTileStorageConfigStatic.GetTileFileExt: string;
begin
  Result := FTileFileExt;
end;

end.
