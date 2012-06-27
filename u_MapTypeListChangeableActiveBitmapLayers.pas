unit u_MapTypeListChangeableActiveBitmapLayers;

interface

uses
  SysUtils,
  i_JclNotify,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeListChangeable,
  u_ChangeableBase;

type
  TMapTypeListChangeableActiveBitmapLayers = class(TChangeableBase, IMapTypeListChangeable)
  private
    FActiveMaps: IActivMapWithLayers;

    FLayersSetLock: IReadWriteSync;
    FStaticLock: IReadWriteSync;

    FZOrderListener: IJclListener;
    FLayerSetListener: IJclListener;


    FLayersSet: IMapTypeSet;
    FStatic: IMapTypeListStatic;
    procedure OnMapZOrderChanged;
    procedure OnLayerSetChanged;
  private
    function GetList: IMapTypeListStatic;
  public
    constructor Create(
      AActiveMaps: IActivMapWithLayers
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_NotifyEventListener,
  u_Synchronizer;

{ TMapTypeListChangeableActiveBitmapLayers }

constructor TMapTypeListChangeableActiveBitmapLayers.Create(
  AActiveMaps: IActivMapWithLayers);
begin
  inherited Create;
  FActiveMaps := AActiveMaps;
  FLayersSetLock := MakeSyncObj(Self, True);
  FStaticLock := MakeSyncObj(Self, True);

  FZOrderListener := TNotifyNoMmgEventListener.Create(Self.OnMapZOrderChanged);
  FLayerSetListener := TNotifyNoMmgEventListener.Create(Self.OnLayerSetChanged);
  FActiveMaps.GetActiveLayersSet.ChangeNotifier.Add(FLayerSetListener);
  OnLayerSetChanged;
  OnMapZOrderChanged;
end;

destructor TMapTypeListChangeableActiveBitmapLayers.Destroy;
begin
  if FActiveMaps <> nil then begin
    FActiveMaps.GetActiveLayersSet.ChangeNotifier.Remove(FLayerSetListener);
  end;
  FLayerSetListener := nil;
  inherited;
end;

function TMapTypeListChangeableActiveBitmapLayers.GetList: IMapTypeListStatic;
begin
  FStaticLock.BeginRead;
  try
    Result := FStatic;
  finally
    FStaticLock.EndRead;
  end;
end;

procedure TMapTypeListChangeableActiveBitmapLayers.OnLayerSetChanged;
begin

end;

procedure TMapTypeListChangeableActiveBitmapLayers.OnMapZOrderChanged;
begin

end;

end.
