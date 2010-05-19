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
unit u_GUIDList;

interface

uses
  ActiveX,
  i_IGUIDList;

type
  TInterfaceWithGUID = record
    GUID: TGUID;
    Obj: IInterface;
  end;

const
  MaxInterfaceWithGUIDListSize = Maxint div (sizeof(TInterfaceWithGUID) * 2);

type
  PInterfaceWithGUIDList = ^TInterfaceWithGUIDList;
  TInterfaceWithGUIDList = array[0..MaxInterfaceWithGUIDListSize - 1] of TInterfaceWithGUID;
  TInterfaceWithGUIDListSortCompare = function (const Item1, Item2: TGUID): Integer of object;


  TGUIDList = class(TInterfacedObject, IGUIDList)
  protected
    FList: PInterfaceWithGUIDList;
    FCount: Integer;
    FCapacity: Integer;
    FAllowNil: Boolean;
    procedure Grow; virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function GetCount: Integer;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; AGUID: TGUID; AObj: IInterface);
    function Find(AGUID: TGUID; var Index: Integer): Boolean; virtual;
    function CompareGUIDs(const G1, G2: TGUID): Integer; virtual;
    procedure Sort(); virtual;
  public
    constructor Create; overload;
    constructor Create(AAllowNil: Boolean); overload;

    destructor Destroy; override;

    class procedure Error(const Msg: string; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;

    // Добавление объекта. Если объект с таким GUID уже есть, то заменяться не будет
    // Возвращает хранимый объект
    function Add(AGUID: TGUID; AInterface: IInterface): IInterface; virtual;

    // Проверка наличия GUID в списке
    function IsExists(AGUID: TGUID): boolean;

    // Получение объекта по GUID
    function GetByGUID(AGUID: TGUID): IInterface; virtual;

    // Замена существующего объекта новым, если отсутствует, то просто добавится
    procedure Replace(AGUID: TGUID; AInterface: IInterface); virtual;

    // Удаление объекта, если нет с таким GUID, то ничего не будет происходить
    procedure Remove(AGUID: TGUID); virtual;

    // Очитска списка
    procedure Clear; virtual;

    // Получение итератора GUID-ов
    function GetGUIDEnum(): IEnumGUID;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
  end;

resourcestring
  SListIndexError = 'List index out of bounds (%d)';
  SListCapacityError = 'List capacity out of bounds (%d)';
  SListCountError = 'List count out of bounds (%d)';
  SInterfaceIsNilError = 'Interface is nil';

implementation

uses
  Windows,
  Math,
  Classes, SysUtils;

type
  TGUIDListEnum = class(TInterfacedObject, IEnumGUID)
  protected
    FGUIDList: TGUIDList;
    FCurrentIndex: integer;
  public
    constructor Create(AGUIDList: TGUIDList);
    function Next(celt: UINT; out rgelt: TGUID; out pceltFetched: UINT): HResult; stdcall;
    function Skip(celt: UINT): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumGUID): HResult; stdcall;
  end;

{ TGUIDListEnum }

function TGUIDListEnum.Clone(out ppenum: IEnumGUID): HResult;
var
  VGUIDListEnum: TGUIDListEnum;
begin
  VGUIDListEnum := TGUIDListEnum.Create(FGUIDList);
  ppenum := VGUIDListEnum;
  VGUIDListEnum.FCurrentIndex := FCurrentIndex;
  Result := S_OK;
end;

constructor TGUIDListEnum.Create(AGUIDList: TGUIDList);
begin
  FGUIDList := AGUIDList;
  FCurrentIndex := 0;
end;

function TGUIDListEnum.Next(celt: UINT; out rgelt: TGUID;
  out pceltFetched: UINT): HResult;
var
  i: integer;
  VpGUID: PGUID;
begin
  pceltFetched := min(celt, FGUIDList.FCount - FCurrentIndex);
  VpGUID := @rgelt;
  if pceltFetched > 0 then begin
    for i := 0 to pceltFetched - 1 do begin
      VpGUID^ := FGUIDList.FList^[FCurrentIndex + I].GUID;
      Inc(VpGUID);
    end;
    Inc(FCurrentIndex, pceltFetched);
  end;
  if pceltFetched <> celt then begin
    Result := S_FALSE;
  end else begin
    Result := S_OK;
  end;
end;

function TGUIDListEnum.Reset: HResult;
begin
  FCurrentIndex := 0;
  Result := S_OK;
end;

function TGUIDListEnum.Skip(celt: UINT): HResult;
begin
  Inc(FCurrentIndex, celt);
  if FCurrentIndex > FGUIDList.FCount then begin
    Result := S_FALSE;
  end else begin
    Result := S_OK;
  end;
end;

{ TGUIDList }

function TGUIDList.Add(AGUID: TGUID; AInterface: IInterface): IInterface;
var
  VIndex: Integer;
begin
  if (not FAllowNil) and (AInterface = nil) then begin
    raise Exception.Create(LoadResString(@SInterfaceIsNilError));
  end;
  if not Find(AGUID, VIndex) then begin
    Insert(VIndex, AGUID, AInterface);
  end else begin
    Result := FList^[VIndex].Obj;
  end;
end;

procedure TGUIDList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

function TGUIDList.CompareGUIDs(const G1, G2: TGUID): Integer;
begin
  if G1.D1 > G2.D1 then begin
    Result := 1;
  end else begin
    if G1.D1 < G2.D1 then begin
      Result := -1;
    end else begin

      if G1.D2 > G2.D2 then begin
        Result := 1;
      end else begin
        if G1.D2 < G2.D2 then begin
          Result := -1;
        end else begin

          if G1.D3 > G2.D3 then begin
            Result := 1;
          end else begin
            if G1.D3 < G2.D3 then begin
              Result := -1;
            end else begin

              if G1.D4[0] > G2.D4[0] then begin
                Result := 1;
              end else begin
                if G1.D4[0] < G2.D4[0] then begin
                  Result := -1;
                end else begin
                  if G1.D4[1] > G2.D4[1] then begin
                    Result := 1;
                  end else begin
                    if G1.D4[1] < G2.D4[1] then begin
                      Result := -1;
                    end else begin
                      if G1.D4[2] > G2.D4[2] then begin
                        Result := 1;
                      end else begin
                        if G1.D4[2] < G2.D4[2] then begin
                          Result := -1;
                        end else begin

                          if G1.D4[3] > G2.D4[3] then begin
                            Result := 1;
                          end else begin
                            if G1.D4[3] < G2.D4[3] then begin
                              Result := -1;
                            end else begin
                              if G1.D4[4] > G2.D4[4] then begin
                                Result := 1;
                              end else begin
                                if G1.D4[4] < G2.D4[4] then begin
                                  Result := -1;
                                end else begin
                                  if G1.D4[5] > G2.D4[5] then begin
                                    Result := 1;
                                  end else begin
                                    if G1.D4[5] < G2.D4[5] then begin
                                      Result := -1;
                                    end else begin

                                      if G1.D4[6] > G2.D4[6] then begin
                                        Result := 1;
                                      end else begin
                                        if G1.D4[6] < G2.D4[6] then begin
                                          Result := -1;
                                        end else begin
                                          if G1.D4[7] > G2.D4[7] then begin
                                            Result := 1;
                                          end else begin
                                            if G1.D4[7] < G2.D4[7] then begin
                                              Result := -1;
                                            end else begin
                                              Result := 0;
                                            end;
                                          end;
                                        end;
                                      end;

                                    end;
                                  end;
                                end;
                              end;
                            end;
                          end;


                        end;
                      end;
                    end;
                  end;
                end;
              end;

            end;
          end;
        end;
      end;
    end;
  end;
end;

constructor TGUIDList.Create;
begin
  FAllowNil := false;
  inherited;
end;

constructor TGUIDList.Create(AAllowNil: Boolean);
begin
  FAllowNil := AAllowNil;
end;

procedure TGUIDList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then begin
    Error(@SListIndexError, Index);
  end;
  FList^[Index].Obj := nil;
  Dec(FCount);
  if Index < FCount then begin
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TInterfaceWithGUID));
  end;
end;

destructor TGUIDList.Destroy;
begin
  Clear;
  inherited;
end;

class procedure TGUIDList.Error(Msg: PResStringRec; Data: Integer);
begin
  TGUIDList.Error(LoadResString(Msg), Data);
end;

class procedure TGUIDList.Error(const Msg: string; Data: Integer);
  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

function TGUIDList.Find(AGUID: TGUID; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := CompareGUIDs(FList^[I].GUID, AGUID);
    if C < 0 then L := I + 1 else begin
      H := I - 1;
      if C = 0 then begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TGUIDList.GetByGUID(AGUID: TGUID): IInterface;
var
  VIndex: Integer;
begin
  if Find(AGUID, VIndex) then begin
    Result := FList^[VIndex].Obj;
  end else begin
    Result := nil;
  end;
end;

function TGUIDList.IsExists(AGUID: TGUID): boolean;
var
  VIndex: Integer;
begin
  Result := Find(AGUID, VIndex);
end;


function TGUIDList.GetCount: Integer;
begin
  Result := FCount;
end;

function TGUIDList.GetGUIDEnum: IEnumGUID;
begin
  Result := TGUIDListEnum.Create(Self);
end;

procedure TGUIDList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then begin
    Delta := FCapacity div 4;
  end else begin
    if FCapacity > 8 then begin
      Delta := 16;
    end else begin
      Delta := 4;
    end;
  end;
  SetCapacity(FCapacity + Delta);
end;

procedure TGUIDList.Insert(Index: Integer; AGUID: TGUID; AObj: IInterface);
begin
  if (Index < 0) or (Index > FCount) then begin
    Error(@SListIndexError, Index);
  end;
  if FCount = FCapacity then begin
    Grow;
  end;
  if Index < FCount then begin
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TInterfaceWithGUID));
  end;
  FillChar(FList^[Index], SizeOf(TInterfaceWithGUID), 0);
  FList^[Index].GUID := AGUID;
  FList^[Index].Obj := AObj;
  Inc(FCount);
end;

procedure TGUIDList.Remove(AGUID: TGUID);
var
  VIndex: Integer;
begin
  if Find(AGUID, VIndex) then begin
    Delete(VIndex);
  end;
end;

procedure TGUIDList.Replace(AGUID: TGUID; AInterface: IInterface);
var
  VIndex: Integer;
begin
  if (not FAllowNil) and (AInterface = nil) then begin
    raise Exception.Create(LoadResString(@SInterfaceIsNilError));
  end;
  if Find(AGUID, VIndex) then begin
    FList^[VIndex].Obj := AInterface;
  end else begin
    Insert(VIndex, AGUID, AInterface);
  end;
end;

procedure TGUIDList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then begin
    Error(@SListCapacityError, NewCapacity);
  end;
  if NewCapacity <> FCapacity then begin
    ReallocMem(FList, NewCapacity * SizeOf(TInterfaceWithGUID));
    FCapacity := NewCapacity;
  end;
end;

procedure TGUIDList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then begin
    Error(@SListCountError, NewCount);
  end;
  if NewCount > FCapacity then begin
    SetCapacity(NewCount);
  end;
  if NewCount > FCount then begin
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(TInterfaceWithGUID), 0)
  end else begin
    for I := FCount - 1 downto NewCount do begin
      Delete(I);
    end;
  end;
  FCount := NewCount;
end;

procedure QuickSort(SortList: PInterfaceWithGUIDList; L, R: Integer;
  SCompare: TInterfaceWithGUIDListSortCompare);
var
  I, J: Integer;
  P, T: TInterfaceWithGUID;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SCompare(SortList^[I].GUID, P.GUID) < 0 do begin
        Inc(I);
      end;
      while SCompare(SortList^[J].GUID, P.GUID) > 0 do begin
        Dec(J);
      end;
      if I <= J then begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then begin
      QuickSort(SortList, L, J, SCompare);
    end;
    L := I;
  until I >= R;
end;

procedure TGUIDList.Sort();
begin
  if (FList <> nil) and (Count > 0) then begin
    QuickSort(FList, 0, Count - 1, CompareGUIDs);
  end;
end;


end.
