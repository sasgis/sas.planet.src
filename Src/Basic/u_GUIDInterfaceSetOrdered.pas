{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2018, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_GUIDInterfaceSetOrdered;

interface

uses
  ActiveX,
  Classes,
  i_GUIDSet,
  u_GUIDSet;

type
  TInterfaceWithGUID = record
    GUID: TGUID;
    Obj: IInterface;
  end;
  PInterfaceWithGUID = ^TInterfaceWithGUID;

const
  CMaxListSize =
    {$IF CompilerVersion < 23}
    Classes.MaxListSize;
    {$ELSE}
    High(NativeInt) shr 4;
    {$IFEND}

type
  PInterfaceWithGUIDList = ^TInterfaceWithGUIDList;
  TInterfaceWithGUIDList = array[0..CMaxListSize - 1] of PInterfaceWithGUID;

type
  TGUIDInterfaceSetOrdered = class(TGUIDSetBase, IGUIDInterfaceSet)
  private
    FList: PInterfaceWithGUIDList;
    FOrderedList: PInterfaceWithGUIDList;
    procedure Insert(
      Index: Integer;
      const AGUID: TGUID;
      const AObj: IInterface
    );
  protected
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetCount(NewCount: Integer); override;
    procedure Delete(Index: Integer); override;
    function GetItemGUID(Index: Integer): TGUID; override;
    function Find(const AGUID: TGUID; var Index: Integer): Boolean; override;
  private
    function GetItem(Index: Integer): IInterface;
    function GetEnumUnknown: IEnumUnknown;
    // Добавление объекта. Если объект с таким GUID уже есть, то заменяться не будет
    // Возвращает хранимый объект
    function Add(
      const AGUID: TGUID;
      const AInterface: IInterface
    ): IInterface;

    // Получение объекта по GUID
    function GetByGUID(const AGUID: TGUID): IInterface;

    // Замена существующего объекта новым, если отсутствует, то просто добавится
    procedure Replace(
      const AGUID: TGUID;
      const AInterface: IInterface
    );
  end;

resourcestring
  SInterfaceIsNilError = 'Interface is nil';

implementation

uses
  Windows,
  Math,
  SysUtils,
  u_GUIDTool;

type
  TGUIDInterfaceListEnum = class(TInterfacedObject, IEnumUnknown)
  private
    FGUIDList: TGUIDInterfaceSetOrdered;
    FCurrentIndex: integer;
  private
    function Next(
      celt: Longint;
      out rgelt;
      pceltFetched: PLongint
    ): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumUnknown): HResult; stdcall;
  public
    constructor Create(AGUIDList: TGUIDInterfaceSetOrdered);
  end;

{ TGUIDInterfaceListEnum }

function TGUIDInterfaceListEnum.Clone(out ppenum: IEnumUnknown): HResult;
var
  VGUIDListEnum: TGUIDInterfaceListEnum;
begin
  VGUIDListEnum := TGUIDInterfaceListEnum.Create(FGUIDList);
  ppenum := VGUIDListEnum;
  VGUIDListEnum.FCurrentIndex := FCurrentIndex;
  Result := S_OK;
end;

constructor TGUIDInterfaceListEnum.Create(AGUIDList: TGUIDInterfaceSetOrdered);
begin
  inherited Create;
  FGUIDList := AGUIDList;
  FCurrentIndex := 0;
end;

function TGUIDInterfaceListEnum.Next(
  celt: Longint;
  out rgelt;
  pceltFetched: PLongint
): HResult;
var
  i: integer;
  Vp: ^IInterface;
begin
  pceltFetched^ := min(celt, FGUIDList.Count - FCurrentIndex);
  Vp := @rgelt;
  if pceltFetched^ > 0 then begin
    for i := 0 to pceltFetched^ - 1 do begin
      Vp^ := FGUIDList.GetItem(FCurrentIndex + i);
      Inc(Vp);
    end;
    Inc(FCurrentIndex, pceltFetched^);
  end;
  if pceltFetched^ <> celt then begin
    Result := S_FALSE;
  end else begin
    Result := S_OK;
  end;
end;

function TGUIDInterfaceListEnum.Reset: HResult;
begin
  FCurrentIndex := 0;
  Result := S_OK;
end;

function TGUIDInterfaceListEnum.Skip(celt: Longint): HResult;
begin
  Inc(FCurrentIndex, celt);
  if FCurrentIndex > FGUIDList.FCount then begin
    Result := S_FALSE;
  end else begin
    Result := S_OK;
  end;
end;

{ TGUIDList }

function TGUIDInterfaceSetOrdered.Add(
  const AGUID: TGUID;
  const AInterface: IInterface
): IInterface;
var
  VIndex: Integer;
begin
  if (not FAllowNil) and (AInterface = nil) then begin
    raise Exception.Create(LoadResString(@SInterfaceIsNilError));
  end;
  if not Find(AGUID, VIndex) then begin
    Insert(VIndex, AGUID, AInterface);
    Result := AInterface;
  end else begin
    Result := FList[VIndex].Obj;
  end;
end;

procedure TGUIDInterfaceSetOrdered.Delete(Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FCount) then begin
    Error(@SListIndexError, Index);
  end;

  if not Find(FOrderedList[Index].GUID, I) then begin
    I := -1;
  end;

  FOrderedList[Index].Obj := nil;
  Dispose(FOrderedList[Index]);

  Dec(FCount);

  if Index < FCount then begin
    System.Move(
      FOrderedList[Index + 1],
      FOrderedList[Index],
      (FCount - Index) * SizeOf(PInterfaceWithGUID)
    );
  end;

  if (I >= 0) and (I < FCount) then begin
    System.Move(
      FList[I + 1],
      FList[I],
      (FCount - I) * SizeOf(PInterfaceWithGUID)
    );
  end;
end;

function TGUIDInterfaceSetOrdered.Find(const AGUID: TGUID; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := CompareGUIDs(FList[I].GUID, AGUID);
    if C < 0 then begin
      L := I + 1;
    end else begin
      H := I - 1;
      if C = 0 then begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TGUIDInterfaceSetOrdered.GetByGUID(const AGUID: TGUID): IInterface;
var
  VIndex: Integer;
begin
  if Find(AGUID, VIndex) then begin
    Result := FList[VIndex].Obj;
  end else begin
    Result := nil;
  end;
end;

function TGUIDInterfaceSetOrdered.GetEnumUnknown: IEnumUnknown;
begin
  Result := TGUIDInterfaceListEnum.Create(Self);
end;

function TGUIDInterfaceSetOrdered.GetItem(Index: Integer): IInterface;
begin
  Result := FOrderedList[Index].Obj;
end;

function TGUIDInterfaceSetOrdered.GetItemGUID(Index: Integer): TGUID;
begin
  Result := FOrderedList[Index].GUID;
end;

procedure TGUIDInterfaceSetOrdered.Insert(
  Index: Integer;
  const AGUID: TGUID;
  const AObj: IInterface
);
var
  VItem: PInterfaceWithGUID;
begin
  if (Index < 0) or (Index > FCount) then begin
    Error(@SListIndexError, Index);
  end;
  if FCount = FCapacity then begin
    Grow;
  end;
  if Index < FCount then begin
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(PInterfaceWithGUID));
  end;

  New(VItem);
  VItem.GUID := AGUID;
  VItem.Obj := AObj;

  FList[Index] := VItem;
  FOrderedList[FCount] := VItem;

  Inc(FCount);
end;

procedure TGUIDInterfaceSetOrdered.Replace(
  const AGUID: TGUID;
  const AInterface: IInterface
);
var
  VIndex: Integer;
begin
  if (not FAllowNil) and (AInterface = nil) then begin
    raise Exception.Create(LoadResString(@SInterfaceIsNilError));
  end;
  if Find(AGUID, VIndex) then begin
    FList[VIndex].Obj := AInterface;
  end else begin
    Insert(VIndex, AGUID, AInterface);
  end;
end;

procedure TGUIDInterfaceSetOrdered.SetCapacity(NewCapacity: Integer);
var
  VNewSize: Integer;
begin
  if (NewCapacity < FCount) or (NewCapacity > CMaxListSize) then begin
    Error(@SListCapacityError, NewCapacity);
  end;
  if NewCapacity <> FCapacity then begin
    VNewSize := NewCapacity * SizeOf(PInterfaceWithGUID);
    ReallocMem(FList, VNewSize);
    ReallocMem(FOrderedList, VNewSize);
    FCapacity := NewCapacity;
  end;
end;

procedure TGUIDInterfaceSetOrdered.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > CMaxListSize) then begin
    Error(@SListCountError, NewCount);
  end;
  if NewCount > FCapacity then begin
    SetCapacity(NewCount);
  end;
  if NewCount > FCount then begin
    FillChar(FList[FCount], (NewCount - FCount) * SizeOf(PInterfaceWithGUID), 0);
    FillChar(FOrderedList[FCount], (NewCount - FCount) * SizeOf(PInterfaceWithGUID), 0);
  end else begin
    for I := FCount - 1 downto NewCount do begin
      Delete(I);
    end;
  end;
  FCount := NewCount;
end;

end.
