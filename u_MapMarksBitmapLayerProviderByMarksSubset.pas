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

unit u_MapMarksBitmapLayerProviderByMarksSubset;

interface

uses
  Types,
  GR32,
  Graphics,
  WinTypes,
  t_GeoTypes,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_OperationNotifier,
  i_MarksDrawConfig,
  i_MarkPicture,
  i_MarksSimple,
  i_BitmapLayerProvider,
  u_ClipPolygonByRect;

type
  TMapMarksBitmapLayerProviderByMarksSubset = class(TInterfacedObject, IBitmapLayerProvider)
  private
    FConfig: IMarksDrawConfigStatic;
    FMarksSubset: IMarksSubset;
    FBitmapClip: IPolygonClip;

    FTempBmp: TCustomBitmap32;
    FBitmapWithText: TBitmap32;
    FPathPointsOnBitmap: TArrayOfDoublePoint;
    FPathPointsOnBitmapPrepared: TArrayOfDoublePoint;
    FPathFixedPoints: TArrayOfFixedPoint;
    function DrawSubset(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      AMarksSubset: IMarksSubset;
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter
    ): Boolean;
    procedure DrawPath(
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter;
      AMarkLine: IMarkLine
    );
    procedure DrawPoly(
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter;
      AMarkPoly: IMarkPoly
    );
    procedure DrawPoint(
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter;
      AMarkPoint: IMarkPoint
    );
  protected
    function GetBitmapRect(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter
    ): Boolean;
  public
    constructor Create(
      AConfig: IMarksDrawConfigStatic;
      AMarksSubset: IMarksSubset
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  ActiveX,
  SysUtils,
  GR32_Resamplers,
  GR32_Polygons,
  u_GeoFun;

const
  CMaxFontSize = 20;

{ TMapMarksBitmapLayerProviderByMarksSubset }

constructor TMapMarksBitmapLayerProviderByMarksSubset.Create(
  AConfig: IMarksDrawConfigStatic;
  AMarksSubset: IMarksSubset
);
begin
  FConfig := AConfig;
  FMarksSubset := AMarksSubset;

  FTempBmp := TCustomBitmap32.Create;
  FTempBmp.DrawMode := dmBlend;
  FTempBmp.CombineMode := cmMerge;
  FTempBmp.Resampler := TLinearResampler.Create;

  FBitmapWithText := TBitmap32.Create;
  FBitmapWithText.Font.Name := 'Tahoma';
  FBitmapWithText.Font.Style := [];
  FBitmapWithText.DrawMode := dmBlend;
  FBitmapWithText.CombineMode := cmMerge;
  FBitmapWithText.Font.Size := CMaxFontSize;
  FBitmapWithText.Resampler := TLinearResampler.Create;
end;

destructor TMapMarksBitmapLayerProviderByMarksSubset.Destroy;
begin
  FreeAndNil(FTempBmp);
  FreeAndNil(FBitmapWithText);
  inherited;
end;

procedure TMapMarksBitmapLayerProviderByMarksSubset.DrawPath(
  ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter;
  AMarkLine: IMarkLine
);
var
  polygon: TPolygon32;
  i: Integer;
  VPointsCount: Integer;
  VPointsProcessedCount: Integer;
  VLonLat: TDoublePoint;
  VGeoConvert: ICoordConverter;
  VIndex: Integer;
  VScale1: Integer;
  TestArrLenLonLatRect: TDoubleRect;
  TestArrLenPixelRect: TDoubleRect;
begin
  VScale1 := AMarkLine.LineWidth;
  TestArrLenLonLatRect := AMarkLine.LLRect;
  ALocalConverter.GetGeoConverter.CheckLonLatRect(TestArrLenLonLatRect);
  TestArrLenPixelRect := ALocalConverter.LonLatRect2LocalRectFloat(TestArrLenLonLatRect);
  if (abs(TestArrLenPixelRect.Left - TestArrLenPixelRect.Right) > VScale1 + 2) or (abs(TestArrLenPixelRect.Top - TestArrLenPixelRect.Bottom) > VScale1 + 2) then begin
    VGeoConvert := ALocalConverter.GetGeoConverter;
    VPointsCount := Length(AMarkLine.Points);
    if VPointsCount > 0 then begin
      if Length(FPathPointsOnBitmap) < VPointsCount then begin
        SetLength(FPathPointsOnBitmap, VPointsCount);
      end;
      for i := 0 to VPointsCount - 1 do begin
        VLonLat := AMarkLine.Points[i];
        if PointIsEmpty(VLonLat) then begin
          FPathPointsOnBitmap[i] := VLonLat;
        end else begin
          VGeoConvert.CheckLonLatPos(VLonLat);
          FPathPointsOnBitmap[i] := ALocalConverter.LonLat2LocalPixelFloat(VLonLat);
        end;
      end;
      try
        VPointsProcessedCount := FBitmapClip.Clip(FPathPointsOnBitmap[0], VPointsCount, FPathPointsOnBitmapPrepared);
        if VPointsProcessedCount > 0 then begin
          polygon := TPolygon32.Create;
          try
            polygon.Antialiased := true;
            polygon.AntialiasMode := am4times;
            polygon.Closed := False;
            if Length(FPathFixedPoints) < VPointsProcessedCount then begin
              SetLength(FPathFixedPoints, VPointsProcessedCount);
            end;
            VIndex := 0;
            for i := 0 to VPointsProcessedCount - 1 do begin
              if PointIsEmpty(FPathPointsOnBitmapPrepared[i]) then begin
                polygon.AddPoints(FPathFixedPoints[0], VIndex);
                polygon.NewLine;
                VIndex := 0;
              end else begin
                FPathFixedPoints[VIndex] := FixedPoint(FPathPointsOnBitmapPrepared[i].X, FPathPointsOnBitmapPrepared[i].Y);
                Inc(VIndex);
              end;
            end;
            polygon.AddPoints(FPathFixedPoints[0], VIndex);
            with Polygon.Outline do try
              with Grow(GR32.Fixed(AMarkLine.LineWidth / 2), 0.5) do try
                FillMode := pfWinding;
                DrawFill(ATargetBmp, AMarkLine.LineColor);
              finally
                free;
              end;
            finally
              free;
            end;
          finally
            polygon.Free;
          end;
        end;
      except
      end;
    end;
  end;
end;

procedure TMapMarksBitmapLayerProviderByMarksSubset.DrawPoly(
  ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter;
  AMarkPoly: IMarkPoly
);
var
  polygon: TPolygon32;
  i: Integer;
  VPointsCount: Integer;
  VPointsProcessedCount: Integer;
  VLonLat: TDoublePoint;
  VGeoConvert: ICoordConverter;
  VScale1: Integer;
  TestArrLenLonLatRect: TDoubleRect;
  TestArrLenPixelRect: TDoubleRect;
begin
  VScale1 := AMarkPoly.LineWidth;
  TestArrLenLonLatRect := AMarkPoly.LLRect;
  ALocalConverter.GetGeoConverter.CheckLonLatRect(TestArrLenLonLatRect);
  TestArrLenPixelRect := ALocalConverter.LonLatRect2LocalRectFloat(TestArrLenLonLatRect);
  if (abs(TestArrLenPixelRect.Left - TestArrLenPixelRect.Right) > VScale1 + 2) or (abs(TestArrLenPixelRect.Top - TestArrLenPixelRect.Bottom) > VScale1 + 2) then begin
    VGeoConvert := ALocalConverter.GetGeoConverter;
    VPointsCount := Length(AMarkPoly.Points);
    if VPointsCount > 0 then begin
      if Length(FPathPointsOnBitmap) < VPointsCount then begin
        SetLength(FPathPointsOnBitmap, VPointsCount);
      end;
      for i := 0 to VPointsCount - 1 do begin
        VLonLat := AMarkPoly.Points[i];
        VGeoConvert.CheckLonLatPos(VLonLat);
        FPathPointsOnBitmap[i] := ALocalConverter.LonLat2LocalPixelFloat(VLonLat);
      end;
      try
        VPointsProcessedCount := FBitmapClip.Clip(FPathPointsOnBitmap[0], VPointsCount, FPathPointsOnBitmapPrepared);
        if VPointsProcessedCount > 0 then begin
          polygon := TPolygon32.Create;
          try
            polygon.Antialiased := true;
            polygon.AntialiasMode := am4times;
            polygon.Closed := True;
              if Length(FPathFixedPoints) < VPointsProcessedCount then begin
                SetLength(FPathFixedPoints, VPointsProcessedCount);
              end;
              for i := 0 to VPointsProcessedCount - 1 do begin
                FPathFixedPoints[i] := FixedPoint(FPathPointsOnBitmapPrepared[i].X, FPathPointsOnBitmapPrepared[i].Y);
              end;
              polygon.AddPoints(FPathFixedPoints[0], VPointsProcessedCount);
              Polygon.DrawFill(ATargetBmp, AMarkPoly.FillColor);
              with Polygon.Outline do try
                with Grow(GR32.Fixed(AMarkPoly.LineWidth / 2), 0.5) do try
                  FillMode := pfWinding;
                  DrawFill(ATargetBmp, AMarkPoly.BorderColor);
                finally
                  free;
                end;
              finally
                free;
              end;
          finally
            polygon.Free;
          end;
        end;
      except
      end;
    end;
  end;
end;

procedure TMapMarksBitmapLayerProviderByMarksSubset.DrawPoint(
  ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter;
  AMarkPoint: IMarkPoint
);
var
  xy: Tpoint;
  VDstRect: TRect;
  VSrcRect: TRect;
  VTextSize: TSize;
  VMarkSize: Integer;
  VFontSize: Integer;
begin
  VMarkSize := AMarkPoint.MarkerSize;
  VFontSize := AMarkPoint.FontSize;
  xy := ALocalConverter.LonLat2LocalPixel(AMarkPoint.Point);
  if (AMarkPoint.Pic <> nil) then begin
    AMarkPoint.Pic.LoadBitmap(FTempBmp);
    VDstRect := bounds(xy.x - (VMarkSize div 2), xy.y - VMarkSize, VMarkSize, VMarkSize);
    VSrcRect := bounds(0, 0, FTempBmp.Width, FTempBmp.Height);
    ATargetBmp.Draw(VDstRect, VSrcRect, FTempBmp);
  end;
  if FConfig.ShowPointCaption then begin
    if VFontSize > 0 then begin
      FBitmapWithText.MasterAlpha:=AlphaComponent(AMarkPoint.TextColor);
      FBitmapWithText.Font.Size := VFontSize;
      VTextSize := FBitmapWithText.TextExtent(AMarkPoint.Name);
      VTextSize.cx:=VTextSize.cx+2;
      VTextSize.cy:=VTextSize.cy+2;
      FBitmapWithText.SetSize(VTextSize.cx + 2,VTextSize.cy + 2);
      VDstRect.Left := xy.x + (VMarkSize div 2);
      VDstRect.Top := xy.y - (VMarkSize div 2) - VTextSize.cy div 2;
      VDstRect.Right := VDstRect.Left + VTextSize.cx;
      VDstRect.Bottom := VDstRect.Top + VTextSize.cy;
      VSrcRect := bounds(1, 1, VTextSize.cx, VTextSize.cy);
      FBitmapWithText.Clear(0);
      FBitmapWithText.RenderText(2, 2, AMarkPoint.Name, 1, SetAlpha(AMarkPoint.TextBgColor,255));
      FBitmapWithText.RenderText(1, 1, AMarkPoint.Name, 1, SetAlpha(AMarkPoint.TextColor,255));
      ATargetBmp.Draw(VDstRect, VSrcRect, FBitmapWithText);
    end;
  end;
end;

function TMapMarksBitmapLayerProviderByMarksSubset.DrawSubset(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  AMarksSubset: IMarksSubset;
  ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter
): Boolean;
var
  VEnumMarks: IEnumUnknown;
  VMark: IMark;
  i: Cardinal;
  VOldClipRect: TRect;
  VMarkPoint: IMarkPoint;
  VMarkLine: IMarkLine;
  VMarkPoly: IMarkPoly;
begin
  VOldClipRect := ATargetBmp.ClipRect;
  ATargetBmp.ClipRect := ALocalConverter.GetLocalRect;
  try
    VEnumMarks := AMarksSubset.GetEnum;
    if FConfig.UseSimpleDrawOrder then begin
      while (VEnumMarks.Next(1, VMark, @i) = S_OK) do begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Break;
        end;
        if Supports(VMark, IMarkPoint, VMarkPoint) then begin
          DrawPoint(
            ATargetBmp,
            ALocalConverter,
            VMarkPoint
          );
        end else if Supports(VMark, IMarkLine, VMarkLine) then begin
          drawPath(
            ATargetBmp,
            ALocalConverter,
            VMarkLine
          );
        end else if Supports(VMark, IMarkPoly, VMarkPoly) then begin
          DrawPoly(
            ATargetBmp,
            ALocalConverter,
            VMarkPoly
          );
        end;
      end;
    end else begin
      while (VEnumMarks.Next(1, VMark, @i) = S_OK) do begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Break;
        end;
        if Supports(VMark, IMarkPoly, VMarkPoly) then begin
          DrawPoly(
            ATargetBmp,
            ALocalConverter,
            VMarkPoly
          );
        end;
      end;
      VEnumMarks.Reset;
      while (VEnumMarks.Next(1, VMark, @i) = S_OK) do begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Break;
        end;
        if Supports(VMark, IMarkLine, VMarkLine) then begin
          drawPath(
            ATargetBmp,
            ALocalConverter,
            VMarkLine
          );
        end;
      end;
      VEnumMarks.Reset;
      while (VEnumMarks.Next(1, VMark, @i) = S_OK) do begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Break;
        end;
        if Supports(VMark, IMarkPoint, VMarkPoint) then begin
          DrawPoint(
            ATargetBmp,
            ALocalConverter,
            VMarkPoint
          );
        end;
      end;
    end;
  finally
    ATargetBmp.ClipRect := VOldClipRect;
  end;
  Result := True;
end;

function TMapMarksBitmapLayerProviderByMarksSubset.GetBitmapRect(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter
): Boolean;
var
  VRectWithDelta: TRect;
  VLocalRect: TRect;
  VTargetRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMarksSubset: IMarksSubset;
  VDeltaSizeInPixel: TRect;
begin
  VLocalRect := ALocalConverter.GetLocalRect;
  VDeltaSizeInPixel := FConfig.OverSizeRect;
  VRectWithDelta.Left := VLocalRect.Left - VDeltaSizeInPixel.Left;
  VRectWithDelta.Top := VLocalRect.Top - VDeltaSizeInPixel.Top;
  VRectWithDelta.Right := VLocalRect.Right + VDeltaSizeInPixel.Right;
  VRectWithDelta.Bottom := VLocalRect.Bottom + VDeltaSizeInPixel.Bottom;
  VTargetRect := ALocalConverter.LocalRect2MapRectFloat(VRectWithDelta);
  VZoom := ALocalConverter.GetZoom;
  VConverter := ALocalConverter.GetGeoConverter;
  VConverter.CheckPixelRectFloat(VTargetRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VTargetRect, VZoom);
  VMarksSubset := FMarksSubset.GetSubsetByLonLatRect(VLonLatRect);

  FBitmapClip := TPolygonClipByRect.Create(VRectWithDelta);
  Result := DrawSubset(AOperationID, ACancelNotifier, VMarksSubset, ATargetBmp, ALocalConverter);
  FBitmapClip := nil;
end;

end.

