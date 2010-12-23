unit u_WindowLayerBasicList;

interface

uses
  Classes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  u_WindowLayerBasic;

type
  TWindowLayerBasicList = class
  private
    FList: TList;
  protected
    function Get(AIndex: Integer): TWindowLayerAbstract;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItem: TWindowLayerAbstract): Integer;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider);
    procedure StartThreads;
    procedure SendTerminateToThreads;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider);
    property Items[Index: Integer]: TWindowLayerAbstract read Get; default;
  end;

implementation

uses
  SysUtils;

{ TWindowLayerBasicList }

function TWindowLayerBasicList.Add(AItem: TWindowLayerAbstract): Integer;
begin
  Result := FList.Add(AItem);
end;

constructor TWindowLayerBasicList.Create;
begin
  FList := TList.Create;;
end;

destructor TWindowLayerBasicList.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do begin
    Items[i].Free;
  end;
  FreeAndNil(FList);
  inherited;
end;

function TWindowLayerBasicList.Get(AIndex: Integer): TWindowLayerAbstract;
begin
  Result := TWindowLayerAbstract(FList.Items[AIndex]);
end;

procedure TWindowLayerBasicList.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do begin
    Items[i].LoadConfig(AConfigProvider);
  end;
end;

procedure TWindowLayerBasicList.SaveConfig(
  AConfigProvider: IConfigDataWriteProvider);
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do begin
    Items[i].SaveConfig(AConfigProvider);
  end;
end;

procedure TWindowLayerBasicList.SendTerminateToThreads;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do begin
    Items[i].SendTerminateToThreads;
  end;
end;

procedure TWindowLayerBasicList.StartThreads;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do begin
    Items[i].StartThreads;
  end;
end;

end.
