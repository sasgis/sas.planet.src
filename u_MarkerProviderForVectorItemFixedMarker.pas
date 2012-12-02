unit u_MarkerProviderForVectorItemFixedMarker;

interface

uses
  i_MarkerDrawable,
  i_VectorDataItemSimple,
  i_MarkerProviderForVectorItem,
  u_BaseInterfacedObject;

type
  TMarkerProviderForVectorItemFixedMarker = class(TBaseInterfacedObject, IMarkerProviderForVectorItem)
  private
    FMarker: IMarkerDrawableChangeable;
  private
    function GetMarker(const AItem: IVectorDataItemSimple): IMarkerDrawable;
  public
    constructor Create(const AMarker: IMarkerDrawableChangeable);
  end;

implementation

{ TMarkerProviderForVectorItemFixedMarker }

constructor TMarkerProviderForVectorItemFixedMarker.Create(
  const AMarker: IMarkerDrawableChangeable);
begin
  inherited Create;
  FMarker := AMarker;
end;

function TMarkerProviderForVectorItemFixedMarker.GetMarker(
  const AItem: IVectorDataItemSimple): IMarkerDrawable;
begin
  Result := FMarker.GetStatic;
end;

end.
