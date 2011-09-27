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

unit u_FillingMapLayerConfigStatic;

interface

uses
  GR32,
  i_LocalCoordConverter,
  i_MapTypes,
  i_FillingMapLayerConfig;

type
  TFillingMapLayerConfigStatic = class(TInterfacedObject, IFillingMapLayerConfigStatic)
  private
    FVisible: Boolean;
    FSourceMap: IMapType;
    FUseRelativeZoom: Boolean;
    FZoom: Byte;
    FNoTileColor: TColor32;
    FShowTNE: Boolean;
    FTNEColor: TColor32;
  protected
    function GetVisible: Boolean;
    function GetSourceMap: IMapType;
    function GetUseRelativeZoom: Boolean;
    function GetZoom: Byte;
    function GetNoTileColor: TColor32;
    function GetShowTNE: Boolean;
    function GetTNEColor: TColor32;

    function GetActualZoom(ALocalConverter: ILocalCoordConverter): Byte;
  public
    constructor Create(
      AVisible: Boolean;
      ASourceMap: IMapType;
      AUseRelativeZoom: Boolean;
      AZoom: Byte;
      ANoTileColor: TColor32;
      AShowTNE: Boolean;
      ATNEColor: TColor32
    );
  end;

implementation

{ TFillingMapLayerConfigStatic }

constructor TFillingMapLayerConfigStatic.Create(
  AVisible: Boolean;
  ASourceMap: IMapType;
  AUseRelativeZoom: Boolean;
  AZoom: Byte;
  ANoTileColor: TColor32;
  AShowTNE: Boolean;
  ATNEColor: TColor32
);
begin
  FVisible := AVisible;
  FSourceMap := ASourceMap;
  FUseRelativeZoom := AUseRelativeZoom;
  FZoom := AZoom;
  FNoTileColor := ANoTileColor;
  FShowTNE := AShowTNE;
  FTNEColor := ATNEColor;
end;

function TFillingMapLayerConfigStatic.GetActualZoom(
  ALocalConverter: ILocalCoordConverter): Byte;
var
  VZoom: Integer;
begin
  VZoom := FZoom;
  if FUseRelativeZoom then begin
    VZoom := FZoom + ALocalConverter.GetZoom;
  end;
  if VZoom < 0 then begin
    Result := 0;
  end else begin
    Result := VZoom;
    ALocalConverter.GetGeoConverter.CheckZoom(Result);
  end;
end;

function TFillingMapLayerConfigStatic.GetNoTileColor: TColor32;
begin
  Result := FNoTileColor;
end;

function TFillingMapLayerConfigStatic.GetShowTNE: Boolean;
begin
  Result := FShowTNE;
end;

function TFillingMapLayerConfigStatic.GetSourceMap: IMapType;
begin
  Result := FSourceMap;
end;

function TFillingMapLayerConfigStatic.GetZoom: Byte;
begin
  Result := FZoom
end;

function TFillingMapLayerConfigStatic.GetTNEColor: TColor32;
begin
  Result := FTNEColor;
end;

function TFillingMapLayerConfigStatic.GetUseRelativeZoom: Boolean;
begin
  Result := FUseRelativeZoom;
end;

function TFillingMapLayerConfigStatic.GetVisible: Boolean;
begin
  Result := FVisible;
end;

end.
