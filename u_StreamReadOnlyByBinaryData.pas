unit u_StreamReadOnlyByBinaryData;

interface

uses
  Classes,
  i_BinaryData;

type
  TStreamReadOnlyByBinaryData = class(TCustomMemoryStream)
  private
    FData: IBinaryData;
  public
    procedure SetSize(NewSize: Longint); override;
    function Write(
      const Buffer;
      Count: Longint
    ): Longint; override;
  public
    constructor Create(const AData: IBinaryData);
  end;

implementation

{ TStreamReadOnlyByBinaryData }

constructor TStreamReadOnlyByBinaryData.Create(const AData: IBinaryData);
begin
  Assert(AData <> nil);
  inherited Create;
  FData := AData;
  SetPointer(FData.Buffer, FData.Size);
end;

procedure TStreamReadOnlyByBinaryData.SetSize(NewSize: Integer);
begin
  inherited;
  if NewSize <> FData.Size then begin
    raise EWriteError.Create('Read only stream');
  end;
end;

function TStreamReadOnlyByBinaryData.Write(
  const Buffer;
  Count: Integer
): Longint;
begin
  raise EWriteError.Create('Read only stream');
end;

end.
