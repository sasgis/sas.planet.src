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
unit u_GUIDSet;

interface

uses
  Windows,
  ActiveX;

type
  TGUIDSetBase = class(TInterfacedObject)
  protected
    FCount: Integer;
    FCapacity: Integer;
    FAllowNil: Boolean;
    procedure Grow; virtual;
    function GetCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer); virtual; abstract;
    procedure SetCount(NewCount: Integer); virtual; abstract;
    function GetCount: Integer;
    procedure Delete(Index: Integer); virtual; abstract;
    function GetItemGUID(Index: Integer): TGUID; virtual; abstract;
    function Find(
      const AGUID: TGUID;
      var Index: Integer
    ): Boolean; virtual;
    function CompareGUIDs(const G1, G2: TGUID): Integer;
    procedure Sort(); virtual; abstract;
  public
    constructor Create; overload;
    constructor Create(AAllowNil: Boolean); overload;

    destructor Destroy; override;

    class procedure Error(
      const Msg: string;
      Data: Integer
    ); overload; virtual;
    class procedure Error(
      Msg: PResStringRec;
      Data: Integer
    ); overload;

    // Проверка наличия GUID в списке
    function IsExists(const AGUID: TGUID): boolean; virtual;

    // Удаление объекта, если нет с таким GUID, то ничего не будет происходить
    procedure Remove(const AGUID: TGUID); virtual;

    // Очитска списка
    procedure Clear; virtual;

    // Получение итератора GUID-ов
    function GetGUIDEnum(): IEnumGUID; virtual;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
  end;

resourcestring
  SListIndexError = 'List index out of bounds (%d)';
  SListCapacityError = 'List capacity out of bounds (%d)';
  SListCountError = 'List count out of bounds (%d)';

implementation

uses
  Math,
  Classes,
  SysUtils;

type
  TGUIDListEnum = class(TInterfacedObject, IEnumGUID)
  protected
    FGUIDList: TGUIDSetBase;
    FCurrentIndex: integer;
  public
    constructor Create(AGUIDList: TGUIDSetBase);
    function Next(
      celt: UINT;
      out rgelt: TGUID;
      out pceltFetched: UINT
    ): HResult; stdcall;
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

constructor TGUIDListEnum.Create(AGUIDList: TGUIDSetBase);
begin
  inherited Create;
  FGUIDList := AGUIDList;
  FCurrentIndex := 0;
end;

function TGUIDListEnum.Next(
  celt: UINT;
  out rgelt: TGUID;
  out pceltFetched: UINT
): HResult;
var
  i: integer;
  VpGUID: PGUID;
begin
  pceltFetched := min(celt, FGUIDList.Count - FCurrentIndex);
  VpGUID := @rgelt;
  if pceltFetched > 0 then begin
    for i := 0 to pceltFetched - 1 do begin
      VpGUID^ := FGUIDList.GetItemGUID(FCurrentIndex + I);
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

procedure TGUIDSetBase.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

function TGUIDSetBase.CompareGUIDs(const G1, G2: TGUID): Integer;
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

constructor TGUIDSetBase.Create;
begin
  inherited;
  FAllowNil := false;
end;

constructor TGUIDSetBase.Create(AAllowNil: Boolean);
begin
  inherited Create;
  FAllowNil := AAllowNil;
end;

destructor TGUIDSetBase.Destroy;
begin
  Clear;
  inherited;
end;

class procedure TGUIDSetBase.Error(
  Msg: PResStringRec;
  Data: Integer
);
begin
  TGUIDSetBase.Error(LoadResString(Msg), Data);
end;

class procedure TGUIDSetBase.Error(
  const Msg: string;
  Data: Integer
);
  function ReturnAddr: Pointer;
  asm
    MOV     EAX,[EBP+4]
  end;

begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

function TGUIDSetBase.Find(
  const AGUID: TGUID;
  var Index: Integer
): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := CompareGUIDs(GetItemGUID(I), AGUID);
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

function TGUIDSetBase.IsExists(const AGUID: TGUID): boolean;
var
  VIndex: Integer;
begin
  Result := Find(AGUID, VIndex);
end;


function TGUIDSetBase.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TGUIDSetBase.GetCount: Integer;
begin
  Result := FCount;
end;

function TGUIDSetBase.GetGUIDEnum: IEnumGUID;
begin
  Result := TGUIDListEnum.Create(Self);
end;

procedure TGUIDSetBase.Grow;
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

procedure TGUIDSetBase.Remove(const AGUID: TGUID);
var
  VIndex: Integer;
begin
  if Find(AGUID, VIndex) then begin
    Delete(VIndex);
  end;
end;

end.
