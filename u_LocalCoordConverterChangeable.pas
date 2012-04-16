unit u_LocalCoordConverterChangeable;

interface

uses
  SysUtils,
  i_JclNotify,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_LocalCoordConverterFactorySimpe,
  u_ChangeableBase;

type
  TLocalCoordConverterChangeable = class(TChangeableBase, ILocalCoordConverterChangeable)
  private
    FSource: ILocalCoordConverterChangeable;
    FTargetGeoConverter: ICoordConverter;
    FConverterFactory: ILocalCoordConverterFactorySimpe;

    FSourceChangeListener: IJclListener;
    FCS: IReadWriteSync;
    FStatic: ILocalCoordConverter;

    procedure OnSourceChange;
  private
    function GetStatic: ILocalCoordConverter;
  public
    constructor Create(
      const ASource: ILocalCoordConverterChangeable;
      const ATargetGeoConverter: ICoordConverter;
      const AConverterFactory: ILocalCoordConverterFactorySimpe
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  u_NotifyEventListener;

{ TLocalCoordConverterChangeable }

constructor TLocalCoordConverterChangeable.Create(
  const ASource: ILocalCoordConverterChangeable;
  const ATargetGeoConverter: ICoordConverter;
  const AConverterFactory: ILocalCoordConverterFactorySimpe
);
begin
  FSource := ASource;
  FTargetGeoConverter := ATargetGeoConverter;
  FConverterFactory := AConverterFactory;

  FCS := MakeSyncRW_Var(Self);

  FSourceChangeListener := TNotifyNoMmgEventListener.Create(Self.OnSourceChange);
  FSource.ChangeNotifier.Add(FSourceChangeListener);
end;

destructor TLocalCoordConverterChangeable.Destroy;
begin
  if FSource <> nil then begin
    FSource.ChangeNotifier.Remove(FSourceChangeListener);
    FSource := nil;
    FSourceChangeListener := nil;
  end;

  FCS := nil;
  inherited;
end;

function TLocalCoordConverterChangeable.GetStatic: ILocalCoordConverter;
begin
  FCS.BeginRead;
  Try
    Result := FStatic;
  Finally
    FCS.EndRead;
  End;
end;

procedure TLocalCoordConverterChangeable.OnSourceChange;
var
  VSource: ILocalCoordConverter;
  VConverter: ILocalCoordConverter;
  VNeedNotify: Boolean;
begin
  VSource := FSource.GetStatic;
  if VSource <> nil then begin
    if FTargetGeoConverter = nil then begin
      VConverter :=
        FConverterFactory.CreateBySourceWithStableTileRect(
          VSource
        );
    end else begin
      VConverter :=
        FConverterFactory.CreateBySourceWithStableTileRectAndOtherGeo(
          VSource,
          FTargetGeoConverter
        );
    end;
  end else begin
    VConverter := nil;
  end;
  VNeedNotify := False;
  
  FCS.BeginWrite;
  try
    if FStatic = nil then begin
      if VConverter <> nil then begin
        FStatic := VConverter;
        VNeedNotify := True;
      end;
    end else begin
      if not FStatic.GetIsSameConverter(VConverter) then begin
        FStatic := VConverter;
        VNeedNotify := True;
      end;
    end;
  finally
    FCS.EndWrite;
  end;
  
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

end.
