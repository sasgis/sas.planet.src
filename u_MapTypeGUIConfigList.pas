unit u_MapTypeGUIConfigList;

interface

uses
  ActiveX,
  i_GUIDListStatic,
  i_ConfigDataElement,
  i_MapTypeGUIConfigList,
  u_ConfigDataElementComplexBase,
  i_MapTypes;

type
  TMapTypeGUIConfigList = class(TConfigDataElementComplexBase, IMapTypeGUIConfigList)
  private
    FMapsSet: IMapTypeSet;
    FOrderedMapGUIDList: IGUIDListStatic;
    function CreateOrderedList: IGUIDListStatic;
  protected
    procedure DoBeforeChangeNotify; override;
  protected
    function GetOrderedMapGUIDList: IGUIDListStatic;
  public
    constructor Create(
      AMapsSet: IMapTypeSet
    );
  end;

implementation

uses
  u_GUIDListStatic,
  u_MapType;

{ TMapTypeGUIConfigList }

constructor TMapTypeGUIConfigList.Create(AMapsSet: IMapTypeSet);
var
  VGUID: TGUID;
  VEnum: IEnumGUID;
  VGetCount: Cardinal;
  VMap: IMapType;
begin
  inherited Create;
  FMapsSet := AMapsSet;
  FOrderedMapGUIDList := CreateOrderedList;
  VEnum := FMapsSet.GetIterator;
  while VEnum.Next(1, VGUID, VGetCount) = S_OK do begin
    VMap := FMapsSet.GetMapTypeByGUID(VGUID);
    Add(VMap.MapType.GUIConfig, nil);
  end;
end;

function TMapTypeGUIConfigList.CreateOrderedList: IGUIDListStatic;
procedure QuickSort(
  var AIndexList: array of Integer;
  var AGUIDList: array of TGUID;
  L, R: Integer
);
var
  I, J: Integer;
  P: Integer;
  TI: Integer;
  TG: TGUID;
begin
  repeat
    I := L;
    J := R;
    P := AIndexList[(L + R) shr 1];
    repeat
      while AIndexList[I] < P do begin
        Inc(I);
      end;
      while AIndexList[J] > P do begin
        Dec(J);
      end;
      if I <= J then begin
        TI := AIndexList[I];
        TG := AGUIDList[I];

        AIndexList[I] := AIndexList[J];
        AGUIDList[I] := AGUIDList[J];
        AIndexList[J] := TI;
        AGUIDList[J] := TG;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then begin
      QuickSort(AIndexList, AGUIDList, L, J);
    end;
    L := I;
  until I >= R;
end;

var
  VIndexList: array of Integer;
  VGUIDList: array of TGUID;
  VGUID: TGUID;
  VEnum: IEnumGUID;
  VGetCount: Cardinal;
  VMap: IMapType;
  VMapsCount: Integer;
  i: Integer;
begin
  VEnum := FMapsSet.GetIterator;
  VMapsCount := 0;
  while VEnum.Next(1, VGUID, VGetCount) = S_OK do begin
    Inc(VMapsCount);
  end;

  SetLength(VIndexList, VMapsCount);
  SetLength(VGUIDList, VMapsCount);

  VEnum.Reset;
  i := 0;
  while VEnum.Next(1, VGUID, VGetCount) = S_OK do begin
    VMap := FMapsSet.GetMapTypeByGUID(VGUID);
    VIndexList[i] := VMap.MapType.GUIConfig.SortIndex;
    VGUIDList[i] := VMap.GUID;
    Inc(i);
  end;
  QuickSort(VIndexList, VGUIDList, 0, VMapsCount - 1);
  Result := TGUIDListStatic.Create(VGUIDList, VMapsCount);
end;

procedure TMapTypeGUIConfigList.DoBeforeChangeNotify;
begin
  inherited;
  LockWrite;
  try
    FOrderedMapGUIDList := CreateOrderedList;
  finally
    UnlockWrite;
  end;
end;

function TMapTypeGUIConfigList.GetOrderedMapGUIDList: IGUIDListStatic;
begin
  Result := FOrderedMapGUIDList;
end;

end.
