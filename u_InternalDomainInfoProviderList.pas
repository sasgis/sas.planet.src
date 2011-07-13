unit u_InternalDomainInfoProviderList;

interface

uses
  Classes,
  i_InternalDomainInfoProvider;

type
  TInternalDomainInfoProviderList = class(TInterfacedObject, IInternalDomainInfoProviderList)
  private
    FList: TStringList;
  protected
    function GetByName(AName: string): IInternalDomainInfoProvider;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Add(AName: string; ADomain: IInternalDomainInfoProvider);
  end;
implementation

uses
  SysUtils;

{ TInternalDomainInfoProviderList }

constructor TInternalDomainInfoProviderList.Create;
begin
  FList := TStringList.Create;
  FList.Sorted := True;
end;

destructor TInternalDomainInfoProviderList.Destroy;
var
  i: Integer;
  VItem: IInterface;
begin
  for i := 0 to FList.Count - 1 do begin
    VItem := IInterface(Pointer(FList.Objects[i]));
    FList.Objects[i] := nil;
    VItem._Release;
  end;
  FreeAndNil(FList);
  inherited;
end;

procedure TInternalDomainInfoProviderList.Add(AName: string;
  ADomain: IInternalDomainInfoProvider);
var
  VIndex: Integer;
begin
  if not FList.Find(AName, VIndex) then begin
    ADomain._AddRef;
    FList.AddObject(AName, Pointer(ADomain));
  end;
end;

function TInternalDomainInfoProviderList.GetByName(
  AName: string): IInternalDomainInfoProvider;
var
  VIndex: Integer;
begin
  Result := nil;
  if FList.Find(AName, VIndex) then begin
    Result := IInternalDomainInfoProvider(Pointer(FList.Objects[VIndex]));
  end;
end;

end.
