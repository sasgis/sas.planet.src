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
unit i_GUIDSet;

interface

uses
  ActiveX;

type
  IGUIDInterfaceSet = interface(IInterface)
    ['{BA17FFE8-E281-4E2E-8B92-8F39ACC67036}']
    // Добавление объекта. Если объект с таким GUID уже есть, то заменяться не будет
    // Возвращает хранимый объект
    function Add(
      const AGUID: TGUID;
      const AInterface: IInterface
    ): IInterface;

    // Проверка наличия GUID в списке
    function IsExists(const AGUID: TGUID): boolean;

    // Получение объекта по GUID
    function GetByGUID(const AGUID: TGUID): IInterface;

    // Замена существующего объекта новым, если отсутствует, то просто добавится
    procedure Replace(
      const AGUID: TGUID;
      const AInterface: IInterface
    );

    // Удаление объекта, если нет с таким GUID, то ничего не будет происходить
    procedure Remove(const AGUID: TGUID);

    // Очитска списка
    procedure Clear;

    // Получение итератора GUID-ов
    function GetGUIDEnum(): IEnumGUID;
    function GetEnumUnknown: IEnumUnknown;

    function GetItem(AIndex: Integer): IInterface;
    property Items[AIndex: Integer]: IInterface read GetItem;
    procedure SetCount(NewCount: Integer);
    function GetCount: Integer;
    property Count: Integer read GetCount write SetCount;
  end;

  IGUIDObjectSet = interface(IInterface)
    ['{9E176E50-3182-455C-AF58-9B6FB8E30E15}']
    // Добавление объекта. Если объект с таким GUID уже есть, то заменяться не будет
    // Если список является владельцем объектов и переданный объект не равен хранимому, то он будет удален
    // Возвращает хранимый объект
    function Add(
      const AGUID: TGUID;
      AObj: TObject
    ): TObject;

    // Проверка наличия GUID в списке
    function IsExists(const AGUID: TGUID): boolean;

    // Получение объекта по GUID
    function GetByGUID(const AGUID: TGUID): TObject;

    // Замена существующего объекта новым, если отсутствует, то просто добавится
    procedure Replace(
      const AGUID: TGUID;
      AObj: TObject
    );

    // Удаление объекта, если нет с таким GUID, то ничего не будет происходить
    procedure Remove(const AGUID: TGUID);

    // Очитска списка
    procedure Clear;

    // Получение итератора GUID-ов
    function GetGUIDEnum(): IEnumGUID;

    // Является ли этот список владельцем объектов
    function GetIsObjectOwner: Boolean;

    procedure SetCount(NewCount: Integer);
    function GetCount: Integer;
    property Count: Integer read GetCount write SetCount;
  end;

implementation

end.
