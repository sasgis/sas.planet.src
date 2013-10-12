unit u_MapVersionFactoryChangeable;

interface

uses
  i_MapVersionFactory,
  u_ConfigDataElementBase;

type
  TMapVersionFactoryChangeable = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IMapVersionFactoryChangeable, IMapVersionFactoryChangeableInternal)
  private
    FFactory: IMapVersionFactory;
  protected
    function CreateStatic: IInterface; override;
  private
    procedure SetFactory(const AValue: IMapVersionFactory);
  private
    function GetStatic: IMapVersionFactory;
  public
    constructor Create(
      const ADefFactory: IMapVersionFactory
    );
  end;

implementation

{ TMapVersionFactoryChangeable }

constructor TMapVersionFactoryChangeable.Create(
  const ADefFactory: IMapVersionFactory);
begin
  inherited Create;
  FFactory := ADefFactory;
end;

function TMapVersionFactoryChangeable.CreateStatic: IInterface;
begin
  Result := FFactory;
end;

function TMapVersionFactoryChangeable.GetStatic: IMapVersionFactory;
begin
  Result := IMapVersionFactory(GetStaticInternal);
end;

procedure TMapVersionFactoryChangeable.SetFactory(
  const AValue: IMapVersionFactory
);
begin
  LockWrite;
  try
    if not FFactory.IsSameFactoryClass(AValue) then begin
      FFactory := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
