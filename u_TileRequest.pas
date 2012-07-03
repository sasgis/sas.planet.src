unit u_TileRequest;

interface

uses
  Types,
  i_Notify,
  i_NotifierOperation,
  i_TileRequest,
  i_MapVersionInfo;

type
  TTileRequest = class(TInterfacedObject, ITileRequest)
  private
    FTile: TPoint;
    FZoom: Byte;
    FVersionInfo: IMapVersionInfo;
    FStartNotifier: INotifierInternal;
    FFinishNotifier: INotifierInternal;
    FCancelNotifier: INotifierOperation;
    FOperationID: Integer;
  protected
    function GetTile: TPoint;
    function GetZoom: Byte;
    function GetVersionInfo: IMapVersionInfo;
    function GetStartNotifier: INotifierInternal;
    function GetFinishNotifier: INotifierInternal;
    function GetCancelNotifier: INotifierOperation;
    function GetOperationID: Integer;
  public
    constructor Create(
      const ATile: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer
    );
  end;

  TTileRequestWithSizeCheck = class(TTileRequest, ITileRequestWithSizeCheck);

implementation

uses
  u_Notifier;

{ TTileRequest }

constructor TTileRequest.Create(
  const ATile: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer
);
begin
  inherited Create;
  FTile := ATile;
  FZoom := AZoom;
  FVersionInfo := AVersionInfo;
  FFinishNotifier := TNotifierBase.Create;
  FStartNotifier := TNotifierBase.Create;
  FCancelNotifier := ACancelNotifier;
  FOperationID := AOperationID;
end;

function TTileRequest.GetCancelNotifier: INotifierOperation;
begin
  Result := FCancelNotifier;
end;

function TTileRequest.GetFinishNotifier: INotifierInternal;
begin
  Result := FFinishNotifier;
end;

function TTileRequest.GetOperationID: Integer;
begin
  Result := FOperationID;
end;

function TTileRequest.GetStartNotifier: INotifierInternal;
begin
  Result := FStartNotifier;
end;

function TTileRequest.GetTile: TPoint;
begin
  Result := FTile;
end;

function TTileRequest.GetVersionInfo: IMapVersionInfo;
begin
  Result := FVersionInfo;
end;

function TTileRequest.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.


