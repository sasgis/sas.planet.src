{*******************************************************************************

    Version: 0.1
    Copyright (C) 2009 Demydov Viktor
    mailto:vdemidov@gmail.com
    http://viktor.getcv.ru/

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*******************************************************************************}
unit i_IDList;

interface

uses
  ActiveX,
  i_EnumID;

type
  IIDInterfaceList = interface(IInterface)
    ['{9AC37296-C3C6-4272-A829-F5DB3D702382}']
    // Добавление объекта. Если объект с таким ID уже есть, то заменяться не будет
    // Возвращает хранимый объект
    function Add(
      AID: Integer;
      AInterface: IInterface
    ): IInterface;

    // Проверка наличия ID в списке
    function IsExists(AID: Integer): boolean;

    // Получение объекта по ID
    function GetByID(AID: Integer): IInterface;

    // Замена существующего объекта новым, если отсутствует, то просто добавится
    procedure Replace(
      AID: Integer;
      AInterface: IInterface
    );

    // Удаление объекта, если нет с таким GUID, то ничего не будет происходить
    procedure Remove(AID: Integer);

    // Очитска списка
    procedure Clear;

    // Получение итератора ID
    function GetIDEnum(): IEnumID;
    function GetEnumUnknown: IEnumUnknown;

    procedure SetCount(NewCount: Integer);
    function GetCount: Integer;
    property Count: Integer read GetCount write SetCount;
  end;

  IIDObjectList = interface(IInterface)
    ['{52DB5379-FCF5-4681-932E-6B09B141A607}']
    // Добавление объекта. Если объект с таким ID уже есть, то заменяться не будет
    // Если список является владельцем объектов и переданный объект не равен хранимому, то он будет удален
    // Возвращает хранимый объект
    function Add(
      AID: Integer;
      AObj: TObject
    ): TObject;

    // Проверка наличия ID в списке
    function IsExists(AID: Integer): boolean;

    // Получение объекта по ID
    function GetByGUID(AID: Integer): TObject;

    // Замена существующего объекта новым, если отсутствует, то просто добавится
    procedure Replace(
      AID: Integer;
      AObj: TObject
    );

    // Удаление объекта, если нет с таким ID, то ничего не будет происходить
    procedure Remove(AID: Integer);

    // Очитска списка
    procedure Clear;

    // Получение итератора ID
    function GetIDEnum(): IEnumID;

    // Является ли этот список владельцем объектов
    function GetIsObjectOwner: Boolean;

    procedure SetCount(NewCount: Integer);
    function GetCount: Integer;
    property Count: Integer read GetCount write SetCount;
  end;

implementation

end.
