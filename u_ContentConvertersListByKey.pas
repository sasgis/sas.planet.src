unit u_ContentConvertersListByKey;

interface

uses
  Classes,
  i_ContentConverter;

type
  TContentConvertersListByKey = class
  private
    FList: TStringList;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Add(AKey: string; AConverter: IContentConverter);
    function Get(AKey: string): IContentConverter;
  end;

implementation

uses
  SysUtils;

{ TContentConvertersListByKey }

procedure TContentConvertersListByKey.Add(AKey: string;
  AConverter: IContentConverter);
begin
  AConverter._AddRef;
  FList.AddObject(AKey, Pointer(AConverter));
end;

constructor TContentConvertersListByKey.Create;
begin
  FList := TStringList.Create;
  FList.Sorted := True;
  FList.Duplicates := dupError;
end;

destructor TContentConvertersListByKey.Destroy;
var
  i: Integer;
begin
  if FList <> nil then begin
    for i := 0 to FList.Count - 1 do begin
      IInterface(Pointer(FList.Objects[i]))._Release;
    end;
    FreeAndNil(FList);
  end;
  inherited;
end;

function TContentConvertersListByKey.Get(AKey: string): IContentConverter;
var
  VIndex: Integer;
begin
  if FList.Find(AKey, VIndex) then begin
    Result := IContentConverter(Pointer(FList.Objects[VIndex]));
  end else begin
    Result := nil;
  end;
end;

end.
