{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_SortFunc;

interface

uses
  i_InterfaceListSimple;

function IsSortedDoubleArray(
  const AArray: array of Double
): Boolean;
function IsSortedIntegerArray(
  const AArray: array of Integer
): Boolean;

procedure SortDoubleArray(
  var AArray: array of Double
);
procedure SortIntegerArray(
  var AArray: array of Integer
);
procedure SortIntegerArrayByIntegerMeasure(
  var AArray: array of Integer;
  var AMeasure: array of Integer
);
procedure SortInterfaceListByDoubleMeasure(
  const AList: IInterfaceListSimple;
  var AMeasure: array of Double
);
procedure SortInterfaceListByIntegerMeasure(
  const AList: IInterfaceListSimple;
  var AMeasure: array of Integer
);

procedure SortInterfaceListByStringMeasure(
  const AList: IInterfaceListSimple;
  var AMeasure: array of string
);

procedure StableSortInterfaceListByIntegerMeasure(
  const AList: IInterfaceListSimple;
  var AMeasure: array of Integer
);

procedure StableSortInterfaceListByStringMeasure(
  const AList: IInterfaceListSimple;
  var AMeasure: array of string
);

type
  TInterfaceListSortCompareFunction = function (const Item1, Item2: IInterface): Integer;
  TInterfaceListSortCompareFunctor = function (const Item1, Item2: IInterface): Integer of object;

function IsSortedInterfaceListByCompareFunction(
  const AList: IInterfaceListSimple;
  ACompareFunc: TInterfaceListSortCompareFunction
): Boolean;

function IsSortedInterfaceListByCompareFunctor(
  const AList: IInterfaceListSimple;
  ACompareFunc: TInterfaceListSortCompareFunctor
): Boolean;

procedure SortInterfaceListByCompareFunction(
  const AList: IInterfaceListSimple;
  ACompareFunc: TInterfaceListSortCompareFunction
);

procedure SortInterfaceListByCompareFunctor(
  const AList: IInterfaceListSimple;
  ACompareFunc: TInterfaceListSortCompareFunctor
);

implementation

uses
  ExplorerSort;

function IsSortedDoubleArray(
  const AArray: array of Double
): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(AArray) - 1 do begin
    if AArray[i - 1] > AArray[i] then begin
      Result := False;
      Break;
    end;
  end;
end;

function IsSortedIntegerArray(
  const AArray: array of Integer
): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(AArray) - 1 do begin
    if AArray[i - 1] > AArray[i] then begin
      Result := False;
      Break;
    end;
  end;
end;

procedure SortDoubleArray(
  var AArray: array of Double
);
  procedure QuickSort(
    var AArray: array of Double;
    L, R: Integer
  );
  var
    I, J: Integer;
    P: Double;
    T: Double;
  begin
    repeat
      I := L;
      J := R;
      P := AArray[(L + R) shr 1];
      repeat
        while AArray[I] < P do begin
          Inc(I);
        end;
        while AArray[J] > P do begin
          Dec(J);
        end;
        if I <= J then begin
          T := AArray[I];

          AArray[I] := AArray[J];
          AArray[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then begin
        QuickSort(AArray, L, J);
      end;
      L := I;
    until I >= R;
  end;
var
  VCount: Integer;
begin
  VCount := Length(AArray);
  if VCount > 1 then begin
    QuickSort(AArray, 0, VCount - 1);
  end;
end;

procedure SortIntegerArray(
  var AArray: array of Integer
);
  procedure QuickSort(
    var AArray: array of Integer;
    L, R: Integer
  );
  var
    I, J: Integer;
    P: Integer;
    T: Integer;
  begin
    repeat
      I := L;
      J := R;
      P := AArray[(L + R) shr 1];
      repeat
        while AArray[I] < P do begin
          Inc(I);
        end;
        while AArray[J] > P do begin
          Dec(J);
        end;
        if I <= J then begin
          T := AArray[I];

          AArray[I] := AArray[J];
          AArray[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then begin
        QuickSort(AArray, L, J);
      end;
      L := I;
    until I >= R;
  end;
var
  VCount: Integer;
begin
  VCount := Length(AArray);
  if VCount > 1 then begin
    QuickSort(AArray, 0, VCount - 1);
  end;
end;

procedure SortIntegerArrayByIntegerMeasure(
  var AArray: array of Integer;
  var AMeasure: array of Integer
);
  procedure QuickSort(
    var AArray: array of Integer;
    var AMeasure: array of Integer;
    L, R: Integer
  );
  var
    I, J: Integer;
    P: Integer;
    TI: Integer;
    TM: Integer;
  begin
    repeat
      I := L;
      J := R;
      P := AMeasure[(L + R) shr 1];
      repeat
        while AMeasure[I] < P do begin
          Inc(I);
        end;
        while AMeasure[J] > P do begin
          Dec(J);
        end;
        if I <= J then begin
          TM := AArray[I];
          AArray[I] := AArray[J];
          AArray[J] := TM;
          TI := AMeasure[I];
          AMeasure[I] := AMeasure[J];
          AMeasure[J] := TI;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then begin
        QuickSort(AArray, AMeasure, L, J);
      end;
      L := I;
    until I >= R;
  end;
var
  VCount: Integer;
begin
  VCount := Length(AMeasure);
  Assert(Length(AArray) = VCount);
  if VCount > 1 then begin
    QuickSort(AArray, AMeasure, 0, VCount - 1);
  end;
end;

procedure SortInterfaceListByDoubleMeasure(
  const AList: IInterfaceListSimple;
  var AMeasure: array of Double
);
  procedure QuickSort(
    const AList: IInterfaceListSimple;
    var AMeasure: array of Double;
    L, R: Integer
  );
  var
    I, J: Integer;
    P: Double;
    TI: Double;
  begin
    repeat
      I := L;
      J := R;
      P := AMeasure[(L + R) shr 1];
      repeat
        while AMeasure[I] < P do begin
          Inc(I);
        end;
        while AMeasure[J] > P do begin
          Dec(J);
        end;
        if I <= J then begin
          AList.Exchange(I, J);
          TI := AMeasure[I];
          AMeasure[I] := AMeasure[J];
          AMeasure[J] := TI;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then begin
        QuickSort(AList, AMeasure, L, J);
      end;
      L := I;
    until I >= R;
  end;
var
  VCount: Integer;
begin
  Assert(Assigned(AList));
  VCount := Length(AMeasure);
  Assert(AList.Count = VCount);
  if VCount > 1 then begin
    QuickSort(AList, AMeasure, 0, VCount - 1);
  end;
end;

procedure SortInterfaceListByIntegerMeasure(
  const AList: IInterfaceListSimple;
  var AMeasure: array of Integer
);
  procedure QuickSort(
    const AList: IInterfaceListSimple;
    var AMeasure: array of Integer;
    L, R: Integer
  );
  var
    I, J: Integer;
    P: Integer;
    TI: Integer;
  begin
    repeat
      I := L;
      J := R;
      P := AMeasure[(L + R) shr 1];
      repeat
        while AMeasure[I] < P do begin
          Inc(I);
        end;
        while AMeasure[J] > P do begin
          Dec(J);
        end;
        if I <= J then begin
          AList.Exchange(I, J);
          TI := AMeasure[I];
          AMeasure[I] := AMeasure[J];
          AMeasure[J] := TI;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then begin
        QuickSort(AList, AMeasure, L, J);
      end;
      L := I;
    until I >= R;
  end;
var
  VCount: Integer;
begin
  Assert(Assigned(AList));
  VCount := Length(AMeasure);
  Assert(AList.Count = VCount);
  if VCount > 1 then begin
    QuickSort(AList, AMeasure, 0, VCount - 1);
  end;
end;

procedure SortInterfaceListByStringMeasure(
  const AList: IInterfaceListSimple;
  var AMeasure: array of string
);
  procedure QuickSort(
    const AList: IInterfaceListSimple;
    var AMeasure: array of string;
    L, R: Integer
  );
  var
    I, J: Integer;
    P: string;
    TI: string;
  begin
    repeat
      I := L;
      J := R;
      P := AMeasure[(L + R) shr 1];
      repeat
        while CompareStringOrdinal(AMeasure[I], P) < 0 do begin
          Inc(I);
        end;
        while CompareStringOrdinal(AMeasure[J], P) > 0 do begin
          Dec(J);
        end;
        if I <= J then begin
          AList.Exchange(I, J);
          TI := AMeasure[I];
          AMeasure[I] := AMeasure[J];
          AMeasure[J] := TI;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then begin
        QuickSort(AList, AMeasure, L, J);
      end;
      L := I;
    until I >= R;
  end;
var
  VCount: Integer;
begin
  Assert(Assigned(AList));
  VCount := Length(AMeasure);
  Assert(AList.Count = VCount);
  if VCount > 1 then begin
    QuickSort(AList, AMeasure, 0, VCount - 1);
  end;
end;

function IsSortedInterfaceListByCompareFunction(
  const AList: IInterfaceListSimple;
  ACompareFunc: TInterfaceListSortCompareFunction
): Boolean;
var
  i: Integer;
begin
  Assert(Assigned(AList));
  Assert(Assigned(ACompareFunc));
  Result := True;
  for i := 1 to AList.Count - 1 do begin
    if ACompareFunc(AList[i - 1], AList[i]) > 0 then begin
      Result := False;
      Break;
    end;
  end;
end;

function IsSortedInterfaceListByCompareFunctor(
  const AList: IInterfaceListSimple;
  ACompareFunc: TInterfaceListSortCompareFunctor
): Boolean;
var
  i: Integer;
begin
  Assert(Assigned(AList));
  Assert(Assigned(ACompareFunc));
  Result := True;
  for i := 1 to AList.Count - 1 do begin
    if ACompareFunc(AList[i - 1], AList[i]) > 0 then begin
      Result := False;
      Break;
    end;
  end;
end;

procedure SortInterfaceListByCompareFunction(
  const AList: IInterfaceListSimple;
  ACompareFunc: TInterfaceListSortCompareFunction
);
  procedure QuickSort(
    const AList: IInterfaceListSimple;
    ACompareFunc: TInterfaceListSortCompareFunction;
    L, R: Integer
  );
  var
    I, J: Integer;
    P: IInterface;
  begin
    repeat
      I := L;
      J := R;
      P := AList[(L + R) shr 1];
      repeat
        while ACompareFunc(AList[I], P) < 0 do begin
          Inc(I);
        end;
        while ACompareFunc(AList[J], P) > 0 do begin
          Dec(J);
        end;
        if I <= J then begin
          AList.Exchange(I, J);
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then begin
        QuickSort(AList, ACompareFunc, L, J);
      end;
      L := I;
    until I >= R;
  end;
var
  VCount: Integer;
begin
  Assert(Assigned(AList));
  VCount := AList.Count;
  if VCount > 1 then begin
    QuickSort(AList, ACompareFunc, 0, VCount - 1);
  end;
end;

procedure SortInterfaceListByCompareFunctor(
  const AList: IInterfaceListSimple;
  ACompareFunc: TInterfaceListSortCompareFunctor
);
  procedure QuickSort(
    const AList: IInterfaceListSimple;
    ACompareFunc: TInterfaceListSortCompareFunctor;
    L, R: Integer
  );
  var
    I, J: Integer;
    P: IInterface;
  begin
    repeat
      I := L;
      J := R;
      P := AList[(L + R) shr 1];
      repeat
        while ACompareFunc(AList[I], P) < 0 do begin
          Inc(I);
        end;
        while ACompareFunc(AList[J], P) > 0 do begin
          Dec(J);
        end;
        if I <= J then begin
          AList.Exchange(I, J);
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then begin
        QuickSort(AList, ACompareFunc, L, J);
      end;
      L := I;
    until I >= R;
  end;
var
  VCount: Integer;
begin
  Assert(Assigned(AList));
  VCount := AList.Count;
  if VCount > 1 then begin
    QuickSort(AList, ACompareFunc, 0, VCount - 1);
  end;
end;

procedure StableSortInterfaceListByIntegerMeasure(
  const AList: IInterfaceListSimple;
  var AMeasure: array of Integer
);

  procedure DoInsertionSort(const AFirstIndex, ALastIndex: Integer);
  var
    I, J, K: Integer;
  begin
    for I := Succ(AFirstIndex) to ALastIndex do begin
      J := I;
      K := AMeasure[I];
      while (J > AFirstIndex) and (AMeasure[J-1] > K) do begin
        AList.Exchange(J, J-1);
        AMeasure[J] := AMeasure[J-1];
        Dec(J);
      end;
      AMeasure[J] := K;
    end;
  end;

  procedure DoMergeSort(
    const AFirstIndex, ALastIndex: Integer;
    var ATempList: array of IInterface;
    var ATempMeasure: array of Integer
  );
  const
    // When the list is smaller than this we use InsertionSort instead of calling
    // MergeSort recursively.
    // 8 and 64 seem to be the lower and upper limits where the performance degrades, so
    // something between 16 and 32 probably gives the best performance
    CMinListSize = 16;
  var
    I, J: Integer;
    VMid: Integer;
    VToInx: Integer;
    VFirstCount: Integer;
  begin
    // calculate the midpoint
    VMid := (AFirstIndex + ALastIndex) div 2;

    // sort the 1st half of the list, either with merge sort, or, if there are
    // few enough items, with insertion sort
    if AFirstIndex < VMid then begin
      if VMid - AFirstIndex <= CMinListSize then begin
        DoInsertionSort(AFirstIndex, VMid);
      end else begin
        DoMergeSort(AFirstIndex, VMid, ATempList, ATempMeasure);
      end;
    end;

    // sort the 2nd half of the list likewise
    if Succ(VMid) < ALastIndex then begin
      if ALastIndex - Succ(VMid) <= CMinListSize then begin
        DoInsertionSort(Succ(VMid), ALastIndex);
      end else begin
        DoMergeSort(Succ(VMid), ALastIndex, ATempList, ATempMeasure);
      end;
    end;

    // copy the first half of the list to our temporary list
    VFirstCount := Succ(VMid - AFirstIndex);
    for I := 0 to VFirstCount - 1 do begin
      ATempList[I] := AList[AFirstIndex + I];
    end;
    System.Move(AMeasure[AFirstIndex], ATempMeasure[0], VFirstCount * SizeOf(Integer));

    // set up the indexes: i is the index for the temporary list (i.e., the
    // first half of the list), j is the index for the second half of the
    // list, ToInx is the index in the merged where items will be copied
    I := 0;
    J := Succ(VMid);
    VToInx := AFirstIndex;

    // now merge the two lists
    // repeat until one of the lists empties...
    while (I < VFirstCount) and (J <= ALastIndex) do begin
      // calculate the smaller item from the next items in both lists and copy
      // it over; increment the relevant index
      if ATempMeasure[I] <= AMeasure[J] then begin
        AList[VToInx] := ATempList[I];
        AMeasure[VToInx] := ATempMeasure[I];
        Inc(I);
      end else begin
        AList[VToInx] := AList[J];
        AMeasure[VToInx] := AMeasure[J];
        Inc(J);
      end;
      // there's one more item in the merged list
      Inc(VToInx);
    end;

    // if there are any more items in the first list, copy them back over
    if I < VFirstCount then begin
      for J := 0 to VFirstCount - I - 1 do begin
        AList[VToInx + J] := ATempList[I + J];
      end;
      System.Move(ATempMeasure[I], AMeasure[VToInx], (VFirstCount - I) * SizeOf(Integer));
    end;

    // if there are any more items in the second list then they're already in
    // place and we're done; if there aren't, we're still done
  end;

var
  VCount: Integer;
  VTempList: array of IInterface;
  VTempMeasure: array of Integer;
  VTempCount: Integer;
begin
  Assert(Assigned(AList));
  VCount := Length(AMeasure);
  Assert(AList.Count = VCount);
  if VCount > 1 then begin
    VTempCount := (VCount div 2) + 1;
    SetLength(VTempList, VTempCount);
    SetLength(VTempMeasure, VTempCount);
    DoMergeSort(0, VCount - 1, VTempList, VTempMeasure);
  end;
end;

procedure StableSortInterfaceListByStringMeasure(
  const AList: IInterfaceListSimple;
  var AMeasure: array of string
);

  procedure DoInsertionSort(const AFirstIndex, ALastIndex: Integer);
  var
    I, J: Integer;
    K: string;
  begin
    for I := Succ(AFirstIndex) to ALastIndex do begin
      J := I;
      K := AMeasure[I];
      while (J > AFirstIndex) and (CompareStringOrdinal(K, AMeasure[J-1]) < 0) do begin
        AList.Exchange(J, J-1);
        AMeasure[J] := AMeasure[J-1];
        Dec(J);
      end;
      AMeasure[J] := K;
    end;
  end;

  procedure DoMergeSort(
    const AFirstIndex, ALastIndex: Integer;
    var ATempList: array of IInterface;
    var ATempMeasure: array of string
  );
  const
    CMinListSize = 16;
  var
    I, J: Integer;
    VMid: Integer;
    VToInx: Integer;
    VFirstCount: Integer;
  begin
    VMid := (AFirstIndex + ALastIndex) div 2;

    if AFirstIndex < VMid then begin
      if VMid - AFirstIndex <= CMinListSize then begin
        DoInsertionSort(AFirstIndex, VMid);
      end else begin
        DoMergeSort(AFirstIndex, VMid, ATempList, ATempMeasure);
      end;
    end;

    if Succ(VMid) < ALastIndex then begin
      if ALastIndex - Succ(VMid) <= CMinListSize then begin
        DoInsertionSort(Succ(VMid), ALastIndex);
      end else begin
        DoMergeSort(Succ(VMid), ALastIndex, ATempList, ATempMeasure);
      end;
    end;

    VFirstCount := Succ(VMid - AFirstIndex);
    for I := 0 to VFirstCount - 1 do begin
      ATempList[I] := AList[AFirstIndex + I];
      ATempMeasure[I] := AMeasure[AFirstIndex + I];
    end;

    I := 0;
    J := Succ(VMid);
    VToInx := AFirstIndex;

    while (I < VFirstCount) and (J <= ALastIndex) do begin
      if CompareStringOrdinal(ATempMeasure[I], AMeasure[J]) <= 0 then begin
        AList[VToInx] := ATempList[I];
        AMeasure[VToInx] := ATempMeasure[I];
        Inc(I);
      end else begin
        AList[VToInx] := AList[J];
        AMeasure[VToInx] := AMeasure[J];
        Inc(J);
      end;
      Inc(VToInx);
    end;

    if I < VFirstCount then begin
      for J := 0 to VFirstCount - I - 1 do begin
        AList[VToInx + J] := ATempList[I + J];
        AMeasure[VToInx + J] := ATempMeasure[I + J];
      end;
    end;
  end;

var
  VCount: Integer;
  VTempList: array of IInterface;
  VTempMeasure: array of string;
  VTempCount: Integer;
begin
  Assert(Assigned(AList));
  VCount := Length(AMeasure);
  Assert(AList.Count = VCount);
  if VCount > 1 then begin
    VTempCount := (VCount div 2) + 1;
    SetLength(VTempList, VTempCount);
    SetLength(VTempMeasure, VTempCount);
    DoMergeSort(0, VCount - 1, VTempList, VTempMeasure);
  end;
end;

end.
