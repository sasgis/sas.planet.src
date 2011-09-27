{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_BitmapMarkerProviderSimpleArrow;

interface

uses
  Types,
  GR32,
  i_BitmapMarker,
  i_BitmapMarkerProviderSimpleConfig,
  u_BitmapMarkerProviderSimpleBase;

type
  TBitmapMarkerProviderSimpleArrow = class(TBitmapMarkerProviderSimpleBase)
  protected
    function CreateMarker(ASize: Integer): IBitmapMarker; override;
  public
    constructor CreateProvider(
      AConfig: IBitmapMarkerProviderSimpleConfigStatic
    ); override;
  end;

implementation

uses
  GR32_Polygons,
  t_GeoTypes,
  u_GeoFun,
  u_BitmapMarker;

{ TBitmapMarkerProviderSimpleArrow }

constructor TBitmapMarkerProviderSimpleArrow.CreateProvider(
  AConfig: IBitmapMarkerProviderSimpleConfigStatic);
begin
  inherited Create(True, 0, AConfig);
end;

function TBitmapMarkerProviderSimpleArrow.CreateMarker(ASize: Integer): IBitmapMarker;
var
  VConfig: IBitmapMarkerProviderSimpleConfigStatic;
  VMarkerBitmap: TCustomBitmap32;
  VSize: TPoint;
  VPolygon: TPolygon32;
  VCenterPoint: TDoublePoint;
begin
  VMarkerBitmap := TCustomBitmap32.Create;
  try
    VConfig := Config;
    VSize := Point(ASize, ASize);

    VCenterPoint.X := VSize.X / 2;
    VCenterPoint.Y := VSize.Y / 2;

    VMarkerBitmap.SetSize(VSize.Y, VSize.Y);
    VMarkerBitmap.Clear(0);
    VPolygon := TPolygon32.Create;
    try
      VPolygon.Antialiased := true;
      VPolygon.AntialiasMode := am32times;
      VPolygon.Add(FixedPoint(VCenterPoint.X, 0));
      VPolygon.Add(FixedPoint(VCenterPoint.X - VSize.X / 3, VSize.Y - 1));
      VPolygon.Add(FixedPoint(VCenterPoint.X + VSize.X / 3, VSize.Y - 1));
      VPolygon.DrawFill(VMarkerBitmap, VConfig.MarkerColor);
      VPolygon.DrawEdge(VMarkerBitmap, VConfig.BorderColor);
    finally
      VPolygon.Free;
    end;
    Result :=
      TBitmapMarker.Create(
        VMarkerBitmap,
        DoublePoint(VCenterPoint.X, 0),
        True,
        0
      );
  finally
    VMarkerBitmap.Free;
  end;
end;

end.
