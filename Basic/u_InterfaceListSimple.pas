{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_InterfaceListSimple;

interface

uses
  Classes,
  i_InterfaceListStatic,
  i_InterfaceListSimple,
  u_BaseInterfacedObject;

type
  TInterfaceListSimple = class(TBaseInterfacedObject, IInterfaceListSimple)
  private
    FList: TList;
  private
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure Exchange(AIndex1, AIndex2: Integer);
    function First: IInterface;
    function IndexOf(const AItem: IInterface): Integer;
    function Add(const AItem: IInterface): Integer;
    procedure AddList(const AList: IInterfaceList);
    procedure AddListStatic(const AList: IInterfaceListStatic);
    procedure AddListSimple(const AList: IInterfaceListSimple);
    procedure Insert(AIndex: Integer; const AItem: IInterface);
    function Last: IInterface;
    function Remove(const AItem: IInterface): Integer;

    function GetItem(AIndex: Integer): IInterface;
    procedure SetItem(AIndex: Integer; const AItem: IInterface);

    function GetCapacity: Integer;
    procedure SetCapacity(ANewCapacity: Integer);

    function GetCount: Integer;
    procedure SetCount(ANewCount: Integer);

    function MakeStaticAndClear: IInterfaceListStatic;
    function MakeStaticCopy: IInterfaceListStatic;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_InterfaceListStatic;

{ TInterfaceListSimple }

constructor TInterfaceListSimple.Create;
begin
  inherited Create;
  FList := nil;
end;

destructor TInterfaceListSimple.Destroy;
var
  i: Integer;
  VItem: Pointer;
begin
  if Assigned(FList) then begin
    for i := 0 to FList.Count - 1 do begin
      VItem := FList[i];
      if Assigned(VItem) then begin
        IInterface(VItem)._Release;
      end;
    end;
    FreeAndNil(FList);
  end;
  inherited;
end;

function TInterfaceListSimple.Add(const AItem: IInterface): Integer;
begin
  if not Assigned(FList) then begin
    FList := TList.Create;
  end;
  Result := FList.Add(nil);
  FList[Result] := Pointer(AItem);
  if Assigned(AItem) then begin
    AItem._AddRef;
  end;
end;

procedure TInterfaceListSimple.AddList(const AList: IInterfaceList);
var
  i: Integer;
begin
  if Assigned(AList) then begin
    AList.Lock;
    try
      if AList.Count > 0 then begin
        if not Assigned(FList) then begin
          FList := TList.Create;
        end;

        for i := 0 to AList.Count - 1 do begin
          Add(AList[i]);
        end;
      end;
    finally
      AList.Unlock;
    end;
  end;
end;

procedure TInterfaceListSimple.AddListSimple(const AList: IInterfaceListSimple);
var
  i: Integer;
begin
  if Assigned(AList) then begin
    if AList.Count > 0 then begin
      if not Assigned(FList) then begin
        FList := TList.Create;
      end;

      for i := 0 to AList.Count - 1 do begin
        Add(AList[i]);
      end;
    end;
  end;
end;

procedure TInterfaceListSimple.AddListStatic(const AList: IInterfaceListStatic);
var
  i: Integer;
begin
  if Assigned(AList) then begin
    if AList.Count > 0 then begin
      if not Assigned(FList) then begin
        FList := TList.Create;
      end;

      for i := 0 to AList.Count - 1 do begin
        Add(AList[i]);
      end;
    end;
  end;
end;

procedure TInterfaceListSimple.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TInterfaceListSimple.Delete(AIndex: Integer);
var
  VItem: Pointer;
begin
  VItem := FList[AIndex];
  FList.Delete(AIndex);
  if Assigned(VItem) then begin
    IInterface(VItem) := nil;
  end;
end;

procedure TInterfaceListSimple.Exchange(AIndex1, AIndex2: Integer);
begin
  FList.Exchange(AIndex1, AIndex2);
end;

function TInterfaceListSimple.First: IInterface;
begin
  Result := IInterface(FList[0]);
end;

function TInterfaceListSimple.GetCapacity: Integer;
begin
  if Assigned(FList) then begin
    Result := FList.Capacity;
  end else begin
    Result := 0;
  end;
end;

function TInterfaceListSimple.GetCount: Integer;
begin
  if Assigned(FList) then begin
    Result := FList.Count;
  end else begin
    Result := 0;
  end;
end;

function TInterfaceListSimple.GetItem(AIndex: Integer): IInterface;
begin
  Result := IInterface(FList[AIndex]);
end;

function TInterfaceListSimple.IndexOf(const AItem: IInterface): Integer;
begin
  if Assigned(FList) then begin
    Result := FList.IndexOf(Pointer(AItem));
  end else begin
    Result := -1;
  end;
end;

procedure TInterfaceListSimple.Insert(AIndex: Integer; const AItem: IInterface);
begin
  if not Assigned(FList) then begin
    FList := TList.Create;
  end;
  FList.Insert(AIndex, nil);
  FList[AIndex] := Pointer(AItem);
  if Assigned(AItem) then begin
    AItem._AddRef;
  end;
end;

function TInterfaceListSimple.Last: IInterface;
begin
  Result := IInterface(FList[FList.Count - 1]);
end;

function TInterfaceListSimple.MakeStaticAndClear: IInterfaceListStatic;
begin
  Result := nil;
  if Assigned(FList) and (FList.Count > 0) then begin
    Result := TInterfaceListStatic.CreateWithOwn(FList);
  end;
end;

function TInterfaceListSimple.MakeStaticCopy: IInterfaceListStatic;
begin
  Result := nil;
  if Assigned(FList) and (FList.Count > 0) then begin
    Result := TInterfaceListStatic.Create(FList);
  end;
end;

function TInterfaceListSimple.Remove(const AItem: IInterface): Integer;
begin
  Result := FList.IndexOf(Pointer(AItem));
  if Result > -1 then
  begin
    Delete(Result);
  end;
end;

procedure TInterfaceListSimple.SetCapacity(ANewCapacity: Integer);
begin
  if Assigned(FList) then begin
    FList.Capacity := ANewCapacity;
  end else begin
    if ANewCapacity > 0 then begin
      FList := TList.Create;
      FList.Capacity := ANewCapacity;
    end;
  end;
end;

procedure TInterfaceListSimple.SetCount(ANewCount: Integer);
var
  i: Integer;
begin
  if Assigned(FList) then begin
    if FList.Count > ANewCount then begin
      for i := FList.Count - 1 downto ANewCount do begin
        Delete(i);
      end;
    end;
    FList.Count := ANewCount;
  end else begin
    if ANewCount > 0 then begin
      FList := TList.Create;
      FList.Count := ANewCount;
    end;
  end;

end;

procedure TInterfaceListSimple.SetItem(AIndex: Integer; const AItem: IInterface);
var
  VItem: Pointer;
begin
  VItem := FList[AIndex];
  if Assigned(VItem) then begin
    IInterface(VItem) := nil;
  end;
  FList[AIndex] := Pointer(AItem);
  if Assigned(AItem) then begin
    AItem._AddRef;
  end;
end;

end.
