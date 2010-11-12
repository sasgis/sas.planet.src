unit u_ContentConverterMatrix;

interface

uses
  Classes,
  i_IContentConverter;

type
  TContentConverterMatrix = class
  private
    FList: TStringList;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Add(ASourceType, ATargetType: string; AConverter: IContentConverter);
    function Get(ASourceType, ATargetType: string): IContentConverter;
  end;

implementation

uses
  SysUtils,
  u_ContentConvertersListByKey;

{ TContentConverterMatrix }

procedure TContentConverterMatrix.Add(ASourceType, ATargetType: string;
  AConverter: IContentConverter);
var
  VIndex: Integer;
  VList: TContentConvertersListByKey;
begin
  if FList.Find(ASourceType, VIndex) then begin
    VList := TContentConvertersListByKey(FList.Objects[VIndex]);
  end else begin
    VList := TContentConvertersListByKey.Create;
    FList.AddObject(ASourceType, VList);
  end;
  VList.Add(ATargetType, AConverter);
end;

constructor TContentConverterMatrix.Create;
begin
  FList := TStringList.Create;
  FList.Sorted := True;
  FList.Duplicates := dupError;
end;

destructor TContentConverterMatrix.Destroy;
var
  i: Integer;
  VObj: TObject;
begin
  if FList <> nil then begin
    for i := 0 to FList.Count - 1 do begin
      VObj := FList.Objects[i];
      VObj.Free;
    end;
    FreeAndNil(FList);
  end;
  inherited;
end;

function TContentConverterMatrix.Get(ASourceType,
  ATargetType: string): IContentConverter;
var
  VIndex: Integer;
  VList: TContentConvertersListByKey;
begin
  if FList.Find(ASourceType, VIndex) then begin
    VList := TContentConvertersListByKey(FList.Objects[VIndex]);
    Result := VList.Get(ATargetType)
  end else begin
    Result := nil;
  end;
end;

end.
