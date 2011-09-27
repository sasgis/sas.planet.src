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

unit u_WindowLayerBasicList;

interface

uses
  Classes,
  i_InternalPerformanceCounter,
  u_WindowLayerBasic;

type
  TWindowLayerBasicList = class
  private
    FList: TList;
    FPerfList: IInternalPerformanceCounterList;
    function GetCount: Integer;
  protected
    function Get(AIndex: Integer): TWindowLayerAbstract;
  public
    constructor Create(AParentPerfList: IInternalPerformanceCounterList);
    destructor Destroy; override;
    function Add(AItem: TWindowLayerAbstract): Integer;
    procedure StartThreads;
    procedure SendTerminateToThreads;
    property Items[Index: Integer]: TWindowLayerAbstract read Get; default;
    property Count: Integer read GetCount;
  end;

implementation

uses
  SysUtils;

{ TWindowLayerBasicList }

constructor TWindowLayerBasicList.Create(AParentPerfList: IInternalPerformanceCounterList);
begin
  FList := TList.Create;;
  FPerfList := AParentPerfList.CreateAndAddNewSubList('Layer');
end;

destructor TWindowLayerBasicList.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do begin
    Items[i].Free;
  end;
  FreeAndNil(FList);
  inherited;
end;

function TWindowLayerBasicList.Add(AItem: TWindowLayerAbstract): Integer;
begin
  AItem.PerfList := FPerfList.CreateAndAddNewSubList(AItem.ClassName);
  Result := FList.Add(AItem);
end;

function TWindowLayerBasicList.Get(AIndex: Integer): TWindowLayerAbstract;
begin
  Result := TWindowLayerAbstract(FList.Items[AIndex]);
end;

function TWindowLayerBasicList.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TWindowLayerBasicList.SendTerminateToThreads;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do begin
    Items[i].SendTerminateToThreads;
  end;
end;

procedure TWindowLayerBasicList.StartThreads;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do begin
    Items[i].StartThreads;
  end;
end;

end.
