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
  AMapList: array of TMapType;
  AGUIDList: array of TGUID;
  L, R: Integer
);
var
  I, J: Integer;
  PM, TM: TMapType;
  PG, TG: TGUID;
begin
  repeat
    I := L;
    J := R;
    PM := AMapList[(L + R) shr 1];
    PG := AGUIDList[(L + R) shr 1];
    repeat
      while AMapList[I].GUIConfig.SortIndex < PM.GUIConfig.SortIndex do begin
        Inc(I);
      end;
      while AMapList[J].GUIConfig.SortIndex > PM.GUIConfig.SortIndex do begin
        Dec(J);
      end;
      if I <= J then begin
        TM := AMapList[I];
        TG := AGUIDList[I];

        AMapList[I] := AMapList[J];
        AGUIDList[I] := AGUIDList[J];
        AMapList[J] := TM;
        AGUIDList[J] := TG;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then begin
      QuickSort(AMapList, AGUIDList, L, J);
    end;
    L := I;
  until I >= R;
end;

var
  VMapList: array of TMapType;
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

  SetLength(VMapList, VMapsCount);
  SetLength(VGUIDList, VMapsCount);

  VEnum.Reset;
  i := 0;
  while VEnum.Next(1, VGUID, VGetCount) = S_OK do begin
    VMap := FMapsSet.GetMapTypeByGUID(VGUID);
    VMapList[i] := VMap.MapType;
    VGUIDList[i] := VMap.GUID;
    Inc(i);
  end;
  QuickSort(VMapList, VGUIDList, 0, VMapsCount - 1);
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
