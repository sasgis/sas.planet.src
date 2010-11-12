unit u_ContentTypeListByKey;

interface

uses
  Classes,
  i_ContentTypeInfo;

type
  TContentTypeListByKey = class
  private
    FList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AKey: string; AType: IContentTypeInfoBasic);
    function Get(AKey: string):  IContentTypeInfoBasic;
  end;


implementation

uses
  SysUtils;

{ TContentTypeListByKey }

procedure TContentTypeListByKey.Add(AKey: string; AType: IContentTypeInfoBasic);
begin
  FList.AddObject(AKey, Pointer(AType));
end;

constructor TContentTypeListByKey.Create;
begin
  FList := TStringList.Create;
  FList.Sorted := True;
  FList.Duplicates := dupError;
end;

destructor TContentTypeListByKey.Destroy;
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

function TContentTypeListByKey.Get(AKey: string): IContentTypeInfoBasic;
var
  VIndex: Integer;
begin
  if FList.Find(AKey, VIndex) then begin
    Result := IContentTypeInfoBasic(Pointer(FList.Objects[VIndex]));
  end else begin
    Result := nil;
  end;
end;

end.
