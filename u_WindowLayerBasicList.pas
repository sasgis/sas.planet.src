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
    function Get(AIndex: Integer): TWindowLayerBasic;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItem: TWindowLayerBasic): Integer;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider);
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider);
    property Items[Index: Integer]: TWindowLayerBasic read Get; default;
  end;

implementation

uses
  SysUtils;

{ TWindowLayerBasicList }

function TWindowLayerBasicList.Add(AItem: TWindowLayerBasic): Integer;
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

function TWindowLayerBasicList.Get(AIndex: Integer): TWindowLayerBasic;
begin
  Result := TWindowLayerBasic(FList.Items[AIndex]);
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

end.
