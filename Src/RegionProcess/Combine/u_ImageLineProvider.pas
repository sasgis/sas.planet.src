{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_ImageLineProvider;

interface

uses
  Types,
  t_Bitmap32,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_CoordConverter,
  i_ProjectionInfo,
  i_ImageLineProvider,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TImageLineProviderAbstract = class(TBaseInterfacedObject, IImageLineProvider)
  private
    FProjection: IProjectionInfo;
    FMapRect: TRect;
    FImageProvider: IBitmapTileUniProvider;
    FBytesPerPixel: Integer;
    FBgColor: TColor32;

    FZoom: byte;
    FMainGeoConverter: ICoordConverter;

    FPreparedTileRect: TRect;
    FPreparedMapRect: TRect;
    FPreparedData: array of Pointer;

    function GetLocalLine(ALine: Integer): Pointer;
    procedure AddTile(
      const ABitmap: IBitmap32Static;
      const ATile: TPoint
    );
    procedure PrepareBufferMem(ARect: TRect);
    procedure ClearBuffer;
    function GetMapRectForLine(ALine: Integer): TRect;
    procedure PrepareBufferData(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AMapRect: TRect
    );
  protected
    procedure PreparePixleLine(
      ASource: PColor32;
      ATarget: Pointer;
      ACount: Integer
    ); virtual; abstract;
  private
    function GetImageSize: TPoint;
    function GetBytesPerPixel: Integer;
    function GetLine(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      ALine: Integer
    ): Pointer;
  public
    constructor Create(
      const AImageProvider: IBitmapTileUniProvider;
      const AProjection: IProjectionInfo;
      const AMapRect: TRect;
      ABgColor: TColor32;
      ABytesPerPixel: Integer
    );
    destructor Destroy; override;
  end;

  TImageLineProviderNoAlfa = class(TImageLineProviderAbstract)
  public
    constructor Create(
      const AImageProvider: IBitmapTileUniProvider;
      const AProjection: IProjectionInfo;
      const AMapRect: TRect;
      ABgColor: TColor32
    );
  end;

  TImageLineProviderWithAlfa = class(TImageLineProviderAbstract)
  public
    constructor Create(
      const AImageProvider: IBitmapTileUniProvider;
      const AProjection: IProjectionInfo;
      const AMapRect: TRect;
      ABgColor: TColor32
    );
  end;

  TImageLineProviderRGB = class(TImageLineProviderNoAlfa)
  protected
    procedure PreparePixleLine(
      ASource: PColor32;
      ATarget: Pointer;
      ACount: Integer
    ); override;
  end;

  TImageLineProviderBGR = class(TImageLineProviderNoAlfa)
  protected
    procedure PreparePixleLine(
      ASource: PColor32;
      ATarget: Pointer;
      ACount: Integer
    ); override;
  end;

  TImageLineProviderRGBA = class(TImageLineProviderWithAlfa)
  protected
    procedure PreparePixleLine(
      ASource: PColor32;
      ATarget: Pointer;
      ACount: Integer
    ); override;
  end;

  TImageLineProviderBGRA = class(TImageLineProviderWithAlfa)
  protected
    procedure PreparePixleLine(
      ASource: PColor32;
      ATarget: Pointer;
      ACount: Integer
    ); override;
  end;

implementation

uses
  t_GeoTypes,
  u_GeoFunc,
  u_TileIteratorByRect;

{ TImageLineProviderAbstract }

constructor TImageLineProviderAbstract.Create(
  const AImageProvider: IBitmapTileUniProvider;
  const AProjection: IProjectionInfo;
  const AMapRect: TRect;
  ABgColor: TColor32;
  ABytesPerPixel: Integer
);
begin
  Assert(Assigned(AImageProvider));
  Assert(Assigned(AProjection));
  inherited Create;
  FImageProvider := AImageProvider;
  FProjection := AProjection;
  FMapRect := AMapRect;
  FBgColor := ABgColor;
  FBytesPerPixel := ABytesPerPixel;

  FZoom := FProjection.Zoom;
  FMainGeoConverter := FProjection.GeoConverter;
end;

destructor TImageLineProviderAbstract.Destroy;
begin
  ClearBuffer;
  inherited;
end;

procedure TImageLineProviderAbstract.AddTile(
  const ABitmap: IBitmap32Static;
  const ATile: TPoint
);
var
  i: Integer;
  VTileMapRect: TRect;
  VTileSize: TPoint;
  VCopyRectSize: TPoint;
  VCopyMapRect: TRect;
  VCopyRectAtSource: TRect;
  VCopyRectAtTarget: TRect;
  VSourceLine: PColor32;
begin
  Assert(Assigned(ABitmap));
  Assert(PtInRect(FPreparedTileRect, ATile));
  VTileMapRect := FMainGeoConverter.TilePos2PixelRect(ATile, FZoom);
  VTileSize := ABitmap.Size;
  Assert(IsPointsEqual(VTileSize, RectSize(VTileMapRect)));
  IntersectRect(VCopyMapRect, VTileMapRect, FPreparedMapRect);

  VCopyRectSize := RectSize(VCopyMapRect);
  VCopyRectAtTarget := RectMove(VCopyMapRect, FPreparedMapRect.TopLeft);
  VCopyRectAtSource := RectMove(VCopyMapRect, VTileMapRect.TopLeft);

  for i := 0 to VCopyRectSize.Y - 1 do begin
    VSourceLine := @ABitmap.Data[VCopyRectAtSource.Left + (i + VCopyRectAtSource.Top) * VTileSize.X];
    PreparePixleLine(
      VSourceLine,
      Pointer(Cardinal(FPreparedData[i + VCopyRectAtTarget.Top]) + Cardinal(VCopyRectAtTarget.Left * FBytesPerPixel)),
      VCopyRectSize.X
    );
  end;
end;

procedure TImageLineProviderAbstract.ClearBuffer;
var
  i: Integer;
begin
  for i := 0 to Length(FPreparedData) - 1 do begin
    if FPreparedData[i] <> nil then begin
      FreeMem(FPreparedData[i]);
      FPreparedData[i] := nil;
    end;
  end;
  FPreparedData := nil;
end;

function TImageLineProviderAbstract.GetBytesPerPixel: Integer;
begin
  Result := FBytesPerPixel;
end;

function TImageLineProviderAbstract.GetImageSize: TPoint;
begin
  Result := RectSize(FMapRect);
end;

function TImageLineProviderAbstract.GetLine(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  ALine: Integer
): Pointer;
begin
  if  not IsRectEmpty(FPreparedMapRect) then begin
    if (ALine < FPreparedMapRect.Top) or (ALine >= FPreparedMapRect.Bottom) then begin
      FPreparedMapRect := Rect(0, 0, 0, 0);
    end;
  end;

  if IsRectEmpty(FPreparedMapRect) then begin
    FPreparedMapRect := GetMapRectForLine(ALine);
    PrepareBufferData(AOperationID, ACancelNotifier, FPreparedMapRect);
  end;
  Result := GetLocalLine(ALine);
end;

function TImageLineProviderAbstract.GetLocalLine(ALine: Integer): Pointer;
var
  VMapLine: Integer;
begin
  Assert(ALine > 0);
  VMapLine := FMapRect.Top + ALine;
  Assert(VMapLine < FMapRect.Bottom);
  Assert(VMapLine >= FPreparedMapRect.Top);
  Assert(VMapLine < FPreparedMapRect.Bottom);
  Result := FPreparedData[VMapLine - FPreparedMapRect.Top];
end;

procedure TImageLineProviderAbstract.PrepareBufferData(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AMapRect: TRect
);
var
  VTile: TPoint;
  VIterator: TTileIteratorByRectRecord;
begin
  PrepareBufferMem(AMapRect);
  FPreparedTileRect := FMainGeoConverter.PixelRect2TileRect(AMapRect, FZoom);
  VIterator.Init(FPreparedTileRect);
  while VIterator.Next(VTile) do begin
    AddTile(
      FImageProvider.GetTile(AOperationID, ACancelNotifier, FProjection, VTile),
      VTile
    );
  end;
end;

procedure TImageLineProviderAbstract.PrepareBufferMem(ARect: TRect);
var
  VLinesExists: Integer;
  VLinesNeed: Integer;
  VWidth: Integer;
  i: Integer;
begin
  VWidth := ARect.Right - ARect.Left;
  VLinesNeed := ARect.Bottom - ARect.Top;
  VLinesExists := Length(FPreparedData);
  if VLinesExists < VLinesNeed then begin
    SetLength(FPreparedData, VLinesNeed);
    for i := VLinesExists to VLinesNeed - 1 do begin
      GetMem(FPreparedData[i], (VWidth + 1) * FBytesPerPixel);
    end;
  end;
end;

function TImageLineProviderAbstract.GetMapRectForLine(ALine: Integer): TRect;
var
  VMapLine: Integer;
  VTilePos: TPoint;
  VPixelRect: TRect;
begin
  Assert(ALine > 0);
  VMapLine := FMapRect.Top + ALine;
  Assert(VMapLine < FMapRect.Bottom);
  VTilePos := FMainGeoConverter.PixelPos2TilePos(Point(FMapRect.Left, VMapLine), FZoom, prToTopLeft);
  VPixelRect := FMainGeoConverter.TilePos2PixelRect(VTilePos, FZoom);
  Result := Rect(FMapRect.Left, VPixelRect.Top, FMapRect.Right, VPixelRect.Bottom);
end;

{ TImageLineProviderNoAlfa }

constructor TImageLineProviderNoAlfa.Create(
  const AImageProvider: IBitmapTileUniProvider;
  const AProjection: IProjectionInfo;
  const AMapRect: TRect;
  ABgColor: TColor32
);
begin
  inherited Create(
    AImageProvider,
    AProjection,
    AMapRect,
    ABgColor,
    3
  );
end;

{ TImageLineProviderWithAlfa }

constructor TImageLineProviderWithAlfa.Create(
  const AImageProvider: IBitmapTileUniProvider;
  const AProjection: IProjectionInfo;
  const AMapRect: TRect;
  ABgColor: TColor32
);
begin
  inherited Create(
    AImageProvider,
    AProjection,
    AMapRect,
    ABgColor,
    4
  );
end;

type
  TBGR = packed record
    B: Byte;
    G: Byte;
    R: Byte;
  end;

  TRGB = packed record
    R: Byte;
    G: Byte;
    B: Byte;
  end;

  TRGBA = packed record
    R: Byte;
    G: Byte;
    B: Byte;
    A: Byte;
  end;



{ TImageLineProviderRGB }

procedure TImageLineProviderRGB.PreparePixleLine(
  ASource: PColor32;
  ATarget: Pointer;
  ACount: Integer
);
var
  i: Integer;
  VSource: PColor32Entry;
  VTarget: ^TRGB;
begin
  if ASource <> nil then begin
    VSource := PColor32Entry(ASource);
    VTarget := ATarget;
    for i := 0 to ACount - 1 do begin
      VTarget.B := VSource.B;
      VTarget.G := VSource.G;
      VTarget.R := VSource.R;
      Inc(VSource);
      Inc(VTarget);
    end;
  end else begin
    VTarget := ATarget;
    for i := 0 to ACount - 1 do begin
      VTarget.B := TColor32Entry(FBgColor).B;
      VTarget.G := TColor32Entry(FBgColor).G;
      VTarget.R := TColor32Entry(FBgColor).R;
      Inc(VTarget);
    end;
  end;
end;

{ TImageLineProviderBGR }

procedure TImageLineProviderBGR.PreparePixleLine(
  ASource: PColor32;
  ATarget: Pointer;
  ACount: Integer
);
var
  i: Integer;
  VSource: PColor32Entry;
  VTarget: ^TBGR;
begin
  if ASource <> nil then begin
    VSource := PColor32Entry(ASource);
    VTarget := ATarget;
    for i := 0 to ACount - 1 do begin
      VTarget.B := VSource.B;
      VTarget.G := VSource.G;
      VTarget.R := VSource.R;
      Inc(VSource);
      Inc(VTarget);
    end;
  end else begin
    VTarget := ATarget;
    for i := 0 to ACount - 1 do begin
      VTarget.B := TColor32Entry(FBgColor).B;
      VTarget.G := TColor32Entry(FBgColor).G;
      VTarget.R := TColor32Entry(FBgColor).R;
      Inc(VTarget);
    end;
  end;
end;

{ TImageLineProviderARGB }

procedure TImageLineProviderRGBA.PreparePixleLine(
  ASource: PColor32;
  ATarget: Pointer;
  ACount: Integer
);
var
  i: Integer;
  VSource: PColor32Entry;
  VTarget: ^TRGBA;
begin
  if ASource <> nil then begin
    VSource := PColor32Entry(ASource);
    VTarget := ATarget;
    for i := 0 to ACount - 1 do begin
      VTarget.B := VSource.B;
      VTarget.G := VSource.G;
      VTarget.R := VSource.R;
      VTarget.A := VSource.A;
      Inc(VSource);
      Inc(VTarget);
    end;
  end else begin
    VTarget := ATarget;
    for i := 0 to ACount - 1 do begin
      VTarget.B := TColor32Entry(FBgColor).B;
      VTarget.G := TColor32Entry(FBgColor).G;
      VTarget.R := TColor32Entry(FBgColor).R;
      VTarget.A := TColor32Entry(FBgColor).A;
      Inc(VTarget);
    end;
  end;
end;

{ TImageLineProviderBGRA }

procedure TImageLineProviderBGRA.PreparePixleLine(
  ASource: PColor32;
  ATarget: Pointer;
  ACount: Integer
);
var
  VTarget: PColor32;
  i: Integer;
begin
  if ASource <> nil then begin
    Move(ASource^, ATarget^, ACount * SizeOf(ASource^));
  end else begin
    VTarget := ATarget;
    for i := 0 to ACount - 1 do begin
      VTarget^ := FBgColor;
      Inc(VTarget);
    end;
  end;
end;

end.
