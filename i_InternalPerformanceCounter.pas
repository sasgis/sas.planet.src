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

unit i_InternalPerformanceCounter;

interface

uses
  ActiveX,
  i_IDList;

type
  IInternalPerformanceCounterStaticData = interface
    ['{64BD3E69-4EAB-41CE-8A87-8324FE9E81D2}']
    function GetId: Integer;
    property Id: Integer read GetId;

    function GetName: string;
    property Name: string read GetName;

    function GetCounter: Cardinal;
    property Counter: Cardinal  read GetCounter;

    function GetTotalTime: TDateTime;
    property TotalTime: TDateTime read GetTotalTime;
  end;

  TInternalPerformanceCounterContext = Int64;
  IInternalPerformanceCounter = interface
    ['{2D5EE758-A5EA-467D-A679-C3CD1B116973}']
    function GetId: Integer;
    property Id: Integer read GetId;

    function GetName: string;
    property Name: string read GetName;

    function StartOperation: TInternalPerformanceCounterContext;
    procedure FinishOperation(AContext: TInternalPerformanceCounterContext);

    function GetCounter: Cardinal;
    property Counter: Cardinal  read GetCounter;

    function GetTotalTime: TDateTime;
    property TotalTime: TDateTime read GetTotalTime;

    function GetLastTimeInSeconds: Double;
    property LastTimeInSeconds: Double read GetLastTimeInSeconds;

    function GetStaticData: IInternalPerformanceCounterStaticData;
  end;

  IInternalPerformanceCounterList =  interface
    ['{75567269-AD8D-443F-AA45-9336C9890719}']
    function GetName: string;
    property Name: string read GetName;

    function GetStaticDataList: IIDInterfaceList;
    function GetEunm: IEnumUnknown;
    function CreateAndAddNewCounter(AName: string): IInternalPerformanceCounter;
    function CreateAndAddNewSubList(AName: string): IInternalPerformanceCounterList;
  end;

implementation

end.
