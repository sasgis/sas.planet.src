unit u_GarbageCollectorThread;

interface

uses
  Windows,
  Classes,
  i_IListOfObjectsWithTTL;

type
  TGarbageCollectorThread = class(TThread)
  private
    FList: IListOfObjectsWithTTL;
    FSleepTime: Cardinal;
  protected
    procedure Execute; override;
  public
    constructor Create(AList: IListOfObjectsWithTTL; ASleepTime: Cardinal);
    destructor Destroy; override;
    property List: IListOfObjectsWithTTL read FList;
  end;

implementation

constructor TGarbageCollectorThread.Create(AList: IListOfObjectsWithTTL;
  ASleepTime: Cardinal);
begin
  inherited Create(false);
  FList := AList;
  FSleepTime := ASleepTime;
end;

destructor TGarbageCollectorThread.Destroy;
begin
  FList := nil;
  inherited;
end;

procedure TGarbageCollectorThread.Execute;
var
  VNextCheck: Cardinal;
  VNow: Cardinal;
begin
  VNextCheck := 0;
  while not Terminated do begin
    VNow := GetTickCount;
    if (VNextCheck = 0)
      or (VNextCheck <= VNow)
      or ((VNextCheck > (1 shl 30)) and (VNow < (1 shl 29))) then
    begin
      FList.ProcessObjectsTrim;
      VNextCheck := FList.GetNextCheck;
      if Terminated then begin
        Break;
      end;
      Sleep(FSleepTime);
    end;
  end;
end;

end.
