unit u_MapTypeSetChangeableSimple;

interface

uses
  SysUtils,
  i_MapTypes,
  u_ChangeableBase;

type
  IMapTypeSetChangeableSimpleInternal = interface(IMapTypeSetChangeable)
    procedure SetStatic(const AValue: IMapTypeSet);
  end;

  TMapTypeSetChangeableSimple = class(TChangeableBase, IMapTypeSetChangeable, IMapTypeSetChangeableSimpleInternal)
  private
    FCS: IReadWriteSync;
    FStatic: IMapTypeSet;
  private
    function GetStatic: IMapTypeSet;
  private
    procedure SetStatic(const AValue: IMapTypeSet);
  public
    constructor Create(const ACurrentSet: IMapTypeSet);
  end;

implementation

uses
  u_Synchronizer,
  u_MapTypeSet;

{ TMapTypeSetChangeableSimple }

constructor TMapTypeSetChangeableSimple.Create(const ACurrentSet: IMapTypeSet);
begin
  inherited Create;
  FCS := MakeSyncRW_Var(Self, False);
  SetStatic(ACurrentSet);
end;

function TMapTypeSetChangeableSimple.GetStatic: IMapTypeSet;
begin
  FCS.BeginRead;
  try
    Result := FStatic;
  finally
    FCS.EndRead;
  end;
end;

procedure TMapTypeSetChangeableSimple.SetStatic(const AValue: IMapTypeSet);
var
  VList: IMapTypeSetBuilder;
begin
  FCS.BeginWrite;
  try
    if AValue = nil then begin
      VList := TMapTypeSetBuilder.Create(False);
      FStatic := VList.MakeAndClear;
    end else begin
      FStatic := AValue;
    end;
  finally
    FCS.EndWrite;
  end;
end;

end.
