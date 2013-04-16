unit u_BerkeleyDBMsgLogger;

interface

uses
  Windows,
  Classes,
  SyncObjs,
  u_BaseInterfacedObject;

type
  TBerkeleyDBMsgLogger = class(TObject)
  private
    FMsgCS: TCriticalSection;
    FMsgFileName: string;
    FMsgFileStream: TFileStream;
  public
    procedure SaveVerbMsg(const AMsg: AnsiString);
  public
    constructor Create(const AMsgFileName: string);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

constructor TBerkeleyDBMsgLogger.Create(const AMsgFileName: string);
begin
  inherited Create;
  FMsgFileStream := nil;
  FMsgCS := TCriticalSection.Create;
  FMsgFileName := AMsgFileName;
  if FileExists(FMsgFileName) then begin
    DeleteFile(FMsgFileName); // ignore possible errors
  end;
end;

destructor TBerkeleyDBMsgLogger.Destroy;
begin
  FMsgFileStream.Free;
  FMsgCS.Free;
  inherited Destroy;
end;

procedure TBerkeleyDBMsgLogger.SaveVerbMsg(const AMsg: AnsiString);
var
  VMsg: AnsiString;
  VDateTimeStr: string;
begin
  FMsgCS.Acquire;
  try
    if not Assigned(FMsgFileStream) then begin
      if not FileExists(FMsgFileName) then begin
        FMsgFileStream := TFileStream.Create(FMsgFileName, fmCreate);
        FMsgFileStream.Free;
      end;
      FMsgFileStream := TFileStream.Create(FMsgFileName, fmOpenReadWrite or fmShareDenyNone);
    end;

    DateTimeToString(VDateTimeStr, 'dd-mm-yyyy hh:nn:ss.zzzz', Now);
    VMsg := AnsiString(VDateTimeStr) + #09 + AMsg + #13#10;

    FMsgFileStream.Position := FMsgFileStream.Size;
    FMsgFileStream.Write(VMsg[1], Length(VMsg));
  finally
    FMsgCS.Release;
  end;
end;

end.
