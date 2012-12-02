unit u_StringListStatic;

interface

uses
  Classes,
  i_StringListStatic,
  u_BaseInterfacedObject;

type
  TStringListStatic = class(TBaseInterfacedObject, IStringListStatic)
  private
    FList: TStringList;
  private
    function GetCount: Integer;
    function GetItem(AIndex: Integer): string;
    function IndexOf(const S: string): Integer;
  public
    constructor CreateByStrings(AList: TStrings);
    constructor CreateWithOwn(AList: TStringList);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TStringListStatic }

constructor TStringListStatic.CreateByStrings(AList: TStrings);
var
  VList: TStringList;
begin
  VList := TStringList.Create;
  try
    VList.Assign(AList);
    CreateWithOwn(VList);
    VList := nil;
  finally
    VList.Free;
  end;
end;

constructor TStringListStatic.CreateWithOwn(AList: TStringList);
begin
  inherited Create;
  FList := AList;
end;

destructor TStringListStatic.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TStringListStatic.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TStringListStatic.GetItem(AIndex: Integer): string;
begin
  Result := FList.Strings[AIndex];
end;

function TStringListStatic.IndexOf(const S: string): Integer;
begin
  Result := FList.IndexOf(S);
end;

end.
