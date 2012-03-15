unit u_ThreadPriorityByClass;

interface

uses
  Classes,
  i_ConfigDataProvider;

function GetThreadPriorityByClass(const AThreadPriorityByClass: IConfigDataProvider;
                                  const AObject: TObject): TThreadPriority;

implementation

function GetThreadPriorityByClass(const AThreadPriorityByClass: IConfigDataProvider;
                                  const AObject: TObject): TThreadPriority;
var
  i: Integer;
begin
  if (AThreadPriorityByClass<>nil) and (AObject<>nil) then begin
    // obtain from config
    i := AThreadPriorityByClass.ReadInteger(AObject.ClassName, Ord(tpLower));
    if (i>=Ord(tpIdle)) and (i<=Ord(tpHigher)) then
      Result := TThreadPriority(i)
    else
      Result := tpLower;
  end else begin
    // by default
    Result := tpLower;
  end;
end;

end.