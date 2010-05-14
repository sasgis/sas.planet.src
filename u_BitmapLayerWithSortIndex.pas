unit u_BitmapLayerWithSortIndex;

interface

uses
  GR32_Layers;

type
  TBitmapLayerWithSortIndex = class(TBitmapLayer)
  private
    FSortIndex: Integer;
  public
    constructor Create(ALayerCollection: TLayerCollection; ASortIndex: Integer); reintroduce;
    property SortIndex: Integer read FSortIndex;
  end;
implementation

{ TBitmapLayerWithSortIndex }

constructor TBitmapLayerWithSortIndex.Create(
  ALayerCollection: TLayerCollection; ASortIndex: Integer);
begin
  inherited Create(ALayerCollection);
  FSortIndex := ASortIndex;
end;

end.
 