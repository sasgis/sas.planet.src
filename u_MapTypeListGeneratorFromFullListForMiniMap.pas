unit u_MapTypeListGeneratorFromFullListForMiniMap;

interface

uses
  UMapType,
  u_MapTypeListGeneratorFromFullListBasic;

type
  TMapTypeListGeneratorFromFullListForMiniMap = class(TMapTypeListGeneratorFromFullListBasic)
  protected
    function CheckIsAddMap(AMapType: TMapType): Boolean; override;
  end;

implementation

{ TMapTypeListGeneratorFromFullListForMiniMap }

function TMapTypeListGeneratorFromFullListForMiniMap.CheckIsAddMap(
  AMapType: TMapType): Boolean;
begin
  Result := inherited CheckIsAddMap(AMapType);
  if Result then begin
    if not(AMapType.IsCanShowOnSmMap and AMapType.IsBitmapTiles) then begin
      Result := false;
    end;
  end;
end;

end.
 