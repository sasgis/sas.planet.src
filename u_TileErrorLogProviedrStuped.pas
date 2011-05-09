unit u_TileErrorLogProviedrStuped;

interface

uses
  i_JclNotify,
  i_TileError,
  i_TileErrorLogProviedrStuped;

type
  TTileErrorLogProviedrStuped = class(TInterfacedObject, ITileErrorLogProviedrStuped, ITileErrorLogger)
  private
    FLastErrorInfo: ITileErrorInfo;
    FNotifier: IJclNotifier;
  protected
    function GetLastErrorInfo: ITileErrorInfo;
    function GetNotifier: IJclNotifier;
  protected
    procedure LogError(AValue: ITileErrorInfo);
  public
    constructor Create();
  end;

implementation

uses
  u_JclNotify;

{ TTileErrorLogProviedrStuped }

constructor TTileErrorLogProviedrStuped.Create;
begin
  FNotifier := TJclBaseNotifier.Create;
end;

function TTileErrorLogProviedrStuped.GetLastErrorInfo: ITileErrorInfo;
begin
  Result := FLastErrorInfo;
end;

function TTileErrorLogProviedrStuped.GetNotifier: IJclNotifier;
begin
  Result := FNotifier;
end;

procedure TTileErrorLogProviedrStuped.LogError(AValue: ITileErrorInfo);
begin
  FLastErrorInfo := AValue;
  FNotifier.Notify(nil);
end;

end.
