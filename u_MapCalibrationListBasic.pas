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

unit u_MapCalibrationListBasic;

interface

uses
  Classes,
  i_MapCalibration;

type
  TMapCalibrationListBasic = class(TInterfacedObject, IMapCalibrationList)
  private
    FList: IInterfaceList;
    procedure Add(AItem: IMapCalibration);
  protected
    function GetCount: Integer;
    function Get(AIndex: Integer): IMapCalibration;
  public
    constructor Create();
  end;

implementation

uses
  u_MapCalibrationOzi,
  u_MapCalibrationDat,
  u_MapCalibrationKml,
  u_MapCalibrationTab,
  u_MapCalibrationWorldFiles;

{ TMapCalibrationListBasic }

constructor TMapCalibrationListBasic.Create;
begin
  inherited;
  FList := TInterfaceList.Create;
  Add(TMapCalibrationOzi.Create);
  Add(TMapCalibrationDat.Create);
  Add(TMapCalibrationKml.Create);
  Add(TMapCalibrationTab.Create);
  Add(TMapCalibrationWorldFiles.Create);
end;

procedure TMapCalibrationListBasic.Add(AItem: IMapCalibration);
begin
  FList.Add(AItem);
end;

function TMapCalibrationListBasic.Get(AIndex: Integer): IMapCalibration;
begin
  Result := IMapCalibration(FList.Items[AIndex]);
end;

function TMapCalibrationListBasic.GetCount: Integer;
begin
  Result := FList.Count;
end;

end.
