unit u_LocalCoordConverterChangeable;

interface

uses
  SysUtils,
  i_Notifier,
  i_Listener,
  i_SimpleFlag,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_LocalCoordConverterFactorySimpe,
  u_ConfigDataElementBase;

type
  TLocalCoordConverterChangeable = class(TConfigDataElementBaseEmptySaveLoad, ILocalCoordConverterChangeable, ILocalCoordConverterChangeableInternal)
  private
    FConverter: ILocalCoordConverter;
  private
    function GetStatic: ILocalCoordConverter;
    procedure SetConverter(AValue: ILocalCoordConverter);
  public
    constructor Create(
      const AChangedFlag: ISimpleFlag;
      const ASource: ILocalCoordConverter
    );
  end;

implementation

uses
  u_Synchronizer,
  u_ListenerByEvent;

{ TLocalCoordConverterChangeable }

constructor TLocalCoordConverterChangeable.Create(
  const AChangedFlag: ISimpleFlag;
  const ASource: ILocalCoordConverter
);
begin
  inherited Create(AChangedFlag);
  FConverter := ASource;
end;

function TLocalCoordConverterChangeable.GetStatic: ILocalCoordConverter;
begin
  LockRead;
  Try
    Result := FConverter;
  Finally
    UnlockRead;
  End;
end;

procedure TLocalCoordConverterChangeable.SetConverter(
  AValue: ILocalCoordConverter);
begin
  if AValue = nil then begin
    Exit;
  end;

  LockWrite;
  try
    if not FConverter.GetIsSameConverter(AValue) then begin
      FConverter := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
