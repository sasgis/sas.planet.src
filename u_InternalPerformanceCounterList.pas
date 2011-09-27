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

unit u_InternalPerformanceCounterList;

interface

uses
  Windows,
  Classes,
  ActiveX,
  i_IDList,
  i_InternalPerformanceCounter;

type
  TInternalPerformanceCounterList = class(TInterfacedObject, IInternalPerformanceCounterList)
  private
    FList: IInterfaceList;
    FName: string;
    procedure AppendStaticListByCounterList(AResultList: IIDInterfaceList; ACounterList: IInternalPerformanceCounterList);
  protected
    function GetName: string;

    function GetStaticDataList: IIDInterfaceList;
    function GetEunm: IEnumUnknown;
    function CreateAndAddNewCounter(AName: string): IInternalPerformanceCounter;
    function CreateAndAddNewSubList(AName: string): IInternalPerformanceCounterList;
  public
    constructor Create(AName: string);
  end;

implementation

uses
  SysUtils,
  u_EnumUnknown,
  u_IDInterfaceList,
  u_InternalPerformanceCounter;

{ TInternalPerformanceCounterList }

procedure TInternalPerformanceCounterList.AppendStaticListByCounterList(
  AResultList: IIDInterfaceList; ACounterList: IInternalPerformanceCounterList);
var
  VEnum: IEnumUnknown;
  Vcnt: Integer;
  VUnknown: IInterface;
  VCounter: IInternalPerformanceCounter;
  VList: IInternalPerformanceCounterList;
begin
  VEnum := ACounterList.GetEunm;
  while VEnum.Next(1, VUnknown, @Vcnt)=S_OK do begin
    if Supports(VUnknown, IInternalPerformanceCounter, VCounter) then begin
      AResultList.Add(VCounter.Id, VCounter.GetStaticData);
    end else if Supports(VUnknown, IInternalPerformanceCounterList, VList) then begin
      AppendStaticListByCounterList(AResultList, VList);
    end;
  end;
end;

constructor TInternalPerformanceCounterList.Create(AName: string);
begin
  FList := TInterfaceList.Create;
  FName := AName;
end;

function TInternalPerformanceCounterList.CreateAndAddNewCounter(
  AName: string): IInternalPerformanceCounter;
begin
  Result := TInternalPerformanceCounter.Create(AName);
  FList.Add(Result);
end;

function TInternalPerformanceCounterList.CreateAndAddNewSubList(
  AName: string): IInternalPerformanceCounterList;
begin
  Result := TInternalPerformanceCounterList.Create(AName);
  FList.Add(Result);
end;

function TInternalPerformanceCounterList.GetEunm: IEnumUnknown;
begin
  Result := TEnumUnknown.Create(FList);
end;

function TInternalPerformanceCounterList.GetName: string;
begin
  Result := FName;
end;

function TInternalPerformanceCounterList.GetStaticDataList: IIDInterfaceList;
begin
  Result := TIDInterfaceList.Create;
  AppendStaticListByCounterList(Result, Self);
end;

end.
