unit u_BinaryDataByMemStream;

interface

uses
  Classes,
  i_BinaryData;

type
  TBinaryDataByMemStream = class(TInterfacedObject, IBinaryData)
  private
    FMemStream: TMemoryStream;
  private
    function  GetBuffer: Pointer;
    function GetSize: Integer;
  public
    constructor CreateFromStream(AStream: TStream);
    constructor CreateFromMem(
      const ASize: Integer;
      const ABuffer: Pointer
    );
    constructor CreateWithOwn(AMemStream: TMemoryStream);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TBinaryDataByMemStream }

constructor TBinaryDataByMemStream.CreateFromMem(
  const ASize: Integer;
  const ABuffer: Pointer
);
var
  VStream: TMemoryStream;
begin
  VStream := TMemoryStream.Create;
  try
    VStream.WriteBuffer(ABuffer^, ASize);
  except
    FreeAndNil(VStream);
  end;
  CreateWithOwn(VStream);
end;

constructor TBinaryDataByMemStream.CreateFromStream(AStream: TStream);
var
  VStream: TMemoryStream;
begin
  VStream := TMemoryStream.Create;
  try
    VStream.LoadFromStream(AStream);
  except
    FreeAndNil(VStream);
  end;
  CreateWithOwn(VStream);
end;

constructor TBinaryDataByMemStream.CreateWithOwn(AMemStream: TMemoryStream);
begin
  FMemStream := AMemStream;
end;

destructor TBinaryDataByMemStream.Destroy;
begin
  FreeAndNil(FMemStream);
  inherited;
end;

function TBinaryDataByMemStream.GetBuffer: Pointer;
begin
  Result := FMemStream.Memory;
end;

function TBinaryDataByMemStream.GetSize: Integer;
begin
  Result := FMemStream.Size;
end;

end.
