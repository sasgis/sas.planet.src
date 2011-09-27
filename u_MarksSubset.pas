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

unit u_MarksSubset;

interface

uses
  Classes,
  ActiveX,
  t_GeoTypes,
  i_MarksSimple;

type
  TMarksSubset = class(TInterfacedObject, IMarksSubset)
  private
    FList: IInterfaceList;
  protected
    function GetSubsetByLonLatRect(ARect: TDoubleRect): IMarksSubset;
    function GetEnum: IEnumUnknown;
    function IsEmpty: Boolean;
  public
    constructor Create(AList: IInterfaceList);
  end;

implementation

uses
  u_EnumUnknown;

{ TMarksSubset }

constructor TMarksSubset.Create(AList: IInterfaceList);
begin
  FList := AList;
end;

function TMarksSubset.GetEnum: IEnumUnknown;
begin
  Result := TEnumUnknown.Create(FList);
end;

function TMarksSubset.GetSubsetByLonLatRect(ARect: TDoubleRect): IMarksSubset;
var
  VNewList: IInterfaceList;
  i: Integer;
  VMark: IMark;
  VMarkLonLatRect: TDoubleRect;
begin
  VNewList := TInterfaceList.Create;
  VNewList.Lock;
  try
    for i := 0 to FList.Count - 1 do begin
      VMark := IMark(FList.Items[i]);
      VMarkLonLatRect := VMark.LLRect;
      if(
        (ARect.Right >= VMarkLonLatRect.Left)and
        (ARect.Left <= VMarkLonLatRect.Right)and
        (ARect.Bottom <= VMarkLonLatRect.Top)and
        (ARect.Top >= VMarkLonLatRect.Bottom))
      then begin
        VNewList.Add(VMark);
      end;
    end;
  finally
    VNewList.Unlock;
  end;
  Result := TMarksSubset.Create(VNewList);
end;

function TMarksSubset.IsEmpty: Boolean;
begin
  Result := FList.Count = 0;
end;

end.
