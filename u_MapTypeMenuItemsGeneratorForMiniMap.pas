unit u_MapTypeMenuItemsGeneratorForMiniMap;

interface

uses
  UMapType,
  u_MapTypeMenuItemsGeneratorBasic;

type
  TMapMenuGeneratorForMiniMapCommnon = class(TMapMenuGeneratorBasic)
  protected
    function CheckIsAddMap(AMapType: TMapType): Boolean; override;
  end;

  TMapMenuGeneratorForMiniMapForMaps = class(TMapMenuGeneratorForMiniMapCommnon)
  protected
    function CheckIsAddMap(AMapType: TMapType): Boolean; override;
  end;

  TMapMenuGeneratorForMiniMapForHybr = class(TMapMenuGeneratorForMiniMapCommnon)
  protected
    function CheckIsAddMap(AMapType: TMapType): Boolean; override;
  end;


implementation

{ TMapMenuGeneratorForMiniMapCommnon }

function TMapMenuGeneratorForMiniMapCommnon.CheckIsAddMap(
  AMapType: TMapType): Boolean;
begin
  if AMapType.IsCanShowOnSmMap and AMapType.IsBitmapTiles then begin
    Result := true;
  end else begin
    Result := false;
  end;
end;

{ TMapMenuGeneratorForMiniMapForMaps }

function TMapMenuGeneratorForMiniMapForMaps.CheckIsAddMap(
  AMapType: TMapType): Boolean;
begin
  Result := inherited CheckIsAddMap(AMapType);
  if Result and not AMapType.IsHybridLayer then begin
    Result := true;
  end else begin
    Result := false;
  end;
end;

{ TMapMenuGeneratorForMiniMapForHybr }

function TMapMenuGeneratorForMiniMapForHybr.CheckIsAddMap(
  AMapType: TMapType): Boolean;
begin
  Result := inherited CheckIsAddMap(AMapType);
  if Result and AMapType.IsHybridLayer then begin
    Result := true;
  end else begin
    Result := false;
  end;
end;

end.
 