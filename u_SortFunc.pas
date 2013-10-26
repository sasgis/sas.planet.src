unit u_SortFunc;

interface

uses
  i_InterfaceListSimple;
  
procedure SortInterfaceListByDoubleMeasure(
  const AList: IInterfaceListSimple;
  var AMeasure: array of Double
);
procedure SortInterfaceListByIntegerMeasure(
  const AList: IInterfaceListSimple;
  var AMeasure: array of Integer
);

type
  TInterfaceListSortCompareFunction = function (const Item1, Item2: IInterface): Integer;
  TInterfaceListSortCompareFunctor = function (const Item1, Item2: IInterface): Integer of object;

procedure SortInterfaceListByCompareFunction(
  const AList: IInterfaceListSimple;
  ACompareFunc: TInterfaceListSortCompareFunction
);

procedure SortInterfaceListByCompareFunctor(
  const AList: IInterfaceListSimple;
  ACompareFunc: TInterfaceListSortCompareFunctor
);

implementation

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
    TM: IInterface;
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
          TI := AMeasure[I];
          TM := AList[I];

          AMeasure[I] := AMeasure[J];
          AList[I] := AList[J];
          AMeasure[J] := TI;
          AList[J] := TM;
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
    TM: IInterface;
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
          TI := AMeasure[I];
          TM := AList[I];

          AMeasure[I] := AMeasure[J];
          AList[I] := AList[J];
          AMeasure[J] := TI;
          AList[J] := TM;
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
    TM: IInterface;
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
          TM := AList[I];

          AList[I] := AList[J];
          AList[J] := TM;
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
    TM: IInterface;
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
          TM := AList[I];

          AList[I] := AList[J];
          AList[J] := TM;
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

end.
