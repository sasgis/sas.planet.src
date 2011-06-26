unit u_LastResponseInfo;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_LastResponseInfo,
  u_ConfigDataElementBase;

type
  TLastResponseInfo = class(TConfigDataElementBaseEmptySaveLoad, ILastResponseInfo)
  private
    FResponseHead: string;
  protected
    function GetResponseHead: string;
    procedure SetResponseHead(const AValue: string);
  end;

implementation

{ TLastResponseInfo }

function TLastResponseInfo.GetResponseHead: string;
begin
  LockRead;
  try
    Result := FResponseHead;
  finally
    UnlockRead;
  end;
end;

procedure TLastResponseInfo.SetResponseHead(const AValue: string);
begin
  LockWrite;
  try
    if FResponseHead <> AValue then begin
      FResponseHead := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
