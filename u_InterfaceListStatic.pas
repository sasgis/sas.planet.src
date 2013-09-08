unit u_InterfaceListStatic;

interface

uses
  Classes,
  i_InterfaceListStatic,
  u_BaseInterfacedObject;

type
  TInterfaceListStatic = class(TBaseInterfacedObject, IInterfaceListStatic)
  private
    FList: TList;
  private
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): IInterface;
  public
    constructor Create(AList: TList);
    constructor CreateWithOwn(var AList: TList);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TInterfaceListStatic }

constructor TInterfaceListStatic.Create(AList: TList);
var
  VList: TList;
  i: Integer;
  VItem: Pointer;
begin
  Assert(Assigned(AList));
  VList := TList.Create;
  try
    VList.Capacity := AList.Count;
    for i := 0 to AList.Count - 1 do begin
      VItem := AList[i];
      if Assigned(VItem) then begin
        IInterface(VItem)._AddRef;
      end;
      VList.Add(VItem);
    end;
    CreateWithOwn(VList);
  finally
    if Assigned(VList) then begin
      for i := 0 to VList.Count - 1 do begin
        VItem := VList[i];
        if Assigned(VItem) then begin
          IInterface(VItem)._Release;
        end;
      end;
      VList.Free;
    end;
  end;
end;

constructor TInterfaceListStatic.CreateWithOwn(var AList: TList);
begin
  Assert(Assigned(AList));
  Assert(AList.Count > 0);
  inherited Create;
  FList := AList;
  AList := nil;
end;

destructor TInterfaceListStatic.Destroy;
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

function TInterfaceListStatic.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TInterfaceListStatic.GetItem(const AIndex: Integer): IInterface;
begin
  Result := IInterface(FList[AIndex]);
end;

end.
