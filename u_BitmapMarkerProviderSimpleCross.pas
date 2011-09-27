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

unit u_BitmapMarkerProviderSimpleCross;

interface

uses
  Types,
  GR32,
  i_BitmapMarker,
  i_BitmapMarkerProviderSimpleConfig,
  u_BitmapMarkerProviderSimpleBase;

type
  TBitmapMarkerProviderSimpleCross = class(TBitmapMarkerProviderSimpleBase)
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
  u_BitmapMarker;

{ TBitmapMarkerProviderSimpleCross }

constructor TBitmapMarkerProviderSimpleCross.CreateProvider(
  AConfig: IBitmapMarkerProviderSimpleConfigStatic);
begin
  inherited Create(False, 0, AConfig);
end;

function TBitmapMarkerProviderSimpleCross.CreateMarker(ASize: Integer): IBitmapMarker;
var
  VConfig: IBitmapMarkerProviderSimpleConfigStatic;
  VMarkerBitmap: TCustomBitmap32;
  VSize: TPoint;
  VPolygon: TPolygon32;
  VCenterPoint: TDoublePoint;
  VCrossHalfWidth: Double;
begin
  VMarkerBitmap := TCustomBitmap32.Create;
  try
    VConfig := Config;
    VSize := Point(ASize, ASize);

    VCenterPoint.X := VSize.X / 2;
    VCenterPoint.Y := VSize.Y / 2;

    VCrossHalfWidth := ASize / 10;

    VMarkerBitmap.SetSize(VSize.Y, VSize.Y);
    VMarkerBitmap.Clear(0);
    VPolygon := TPolygon32.Create;
    try
      VPolygon.Antialiased := true;
      VPolygon.Closed := True;
      VPolygon.AntialiasMode := am32times;
      VPolygon.Add(FixedPoint(VCenterPoint.X - VCrossHalfWidth, 0));
      VPolygon.Add(FixedPoint(VCenterPoint.X + VCrossHalfWidth, 0));
      VPolygon.Add(FixedPoint(VCenterPoint.X + VCrossHalfWidth, VCenterPoint.Y - VCrossHalfWidth));
      VPolygon.Add(FixedPoint(VSize.X - 1, VCenterPoint.Y - VCrossHalfWidth));
      VPolygon.Add(FixedPoint(VSize.X - 1, VCenterPoint.Y + VCrossHalfWidth));
      VPolygon.Add(FixedPoint(VCenterPoint.X + VCrossHalfWidth, VCenterPoint.Y + VCrossHalfWidth));
      VPolygon.Add(FixedPoint(VCenterPoint.X + VCrossHalfWidth, VSize.Y - 1));
      VPolygon.Add(FixedPoint(VCenterPoint.X - VCrossHalfWidth, VSize.Y - 1));
      VPolygon.Add(FixedPoint(VCenterPoint.X - VCrossHalfWidth, VCenterPoint.Y + VCrossHalfWidth));
      VPolygon.Add(FixedPoint(0, VCenterPoint.Y + VCrossHalfWidth));
      VPolygon.Add(FixedPoint(0, VCenterPoint.Y - VCrossHalfWidth));
      VPolygon.Add(FixedPoint(VCenterPoint.X - VCrossHalfWidth, VCenterPoint.Y - VCrossHalfWidth));
      VPolygon.DrawFill(VMarkerBitmap, VConfig.MarkerColor);
      VPolygon.DrawEdge(VMarkerBitmap, VConfig.BorderColor);
    finally
      VPolygon.Free;
    end;
    Result :=
      TBitmapMarker.Create(
        VMarkerBitmap,
        VCenterPoint,
        False,
        0
      );
  finally
    VMarkerBitmap.Free;
  end;
end;

end.
