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
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_ImageLineProvider,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TImageLineProviderAbstract = class(TBaseInterfacedObject, IImageLineProvider)
  private
    FImageProvider: IBitmapLayerProvider;
    FConverterFactory: ILocalCoordConverterFactorySimpe;
    FLocalConverter: ILocalCoordConverter;
    FBytesPerPixel: Integer;
    FBgColor: TColor32;

    FZoom: byte;
    FMainGeoConverter: ICoordConverter;
    FPreparedData: array of Pointer;
    FPreparedConverter: ILocalCoordConverter;

    function PrepareConverterForLocalLine(ALine: Integer): ILocalCoordConverter;
    function GetLocalLine(ALine: Integer): Pointer;
    procedure AddTile(
      const ATargetBitmap: IBitmap32Static;
      const AConverter: ILocalCoordConverter
    );
    procedure PrepareBufferMem(ARect: TRect);
    procedure ClearBuffer;
    procedure PrepareBufferData(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AConverter: ILocalCoordConverter
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
      const AImageProvider: IBitmapLayerProvider;
      const ALocalConverter: ILocalCoordConverter;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      ABgColor: TColor32;
      ABytesPerPixel: Integer
    );
    destructor Destroy; override;
  end;

  TImageLineProviderNoAlfa = class(TImageLineProviderAbstract)
  public
    constructor Create(
      const AImageProvider: IBitmapLayerProvider;
      const ALocalConverter: ILocalCoordConverter;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      ABgColor: TColor32
    );
  end;

  TImageLineProviderWithAlfa = class(TImageLineProviderAbstract)
  public
    constructor Create(
      const AImageProvider: IBitmapLayerProvider;
      const ALocalConverter: ILocalCoordConverter;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
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
  u_TileIteratorByRect;

{ TImageLineProviderAbstract }

constructor TImageLineProviderAbstract.Create(
  const AImageProvider: IBitmapLayerProvider;
  const ALocalConverter: ILocalCoordConverter;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  ABgColor: TColor32;
  ABytesPerPixel: Integer
);
begin
  inherited Create;
  FImageProvider := AImageProvider;
  FLocalConverter := ALocalConverter;
  FConverterFactory := AConverterFactory;
  FBgColor := ABgColor;
  FBytesPerPixel := ABytesPerPixel;

  FZoom := FLocalConverter.Zoom;
  FMainGeoConverter := FLocalConverter.GeoConverter;
end;

destructor TImageLineProviderAbstract.Destroy;
begin
  ClearBuffer;
  inherited;
end;

procedure TImageLineProviderAbstract.AddTile(
  const ATargetBitmap: IBitmap32Static;
  const AConverter: ILocalCoordConverter
);
var
  i: Integer;
  VBitmapRect: TRect;
  VPreparedMapRect: TRect;
  VIntersectionAtPrepared: TRect;
  VIntersectionAtBitmap: TRect;
  VSourceLine: PColor32;
begin
  VBitmapRect := AConverter.GetRectInMapPixel;
  VPreparedMapRect := FPreparedConverter.GetRectInMapPixel;
  Assert(VBitmapRect.Top = VPreparedMapRect.Top);
  Assert(VBitmapRect.Bottom = VPreparedMapRect.Bottom);
  Assert(VBitmapRect.Right > VPreparedMapRect.Left);
  Assert(VBitmapRect.Left < VPreparedMapRect.Right);

  if VBitmapRect.Left < VPreparedMapRect.Left then begin
    VBitmapRect.Left := VPreparedMapRect.Left;
  end;
  if VBitmapRect.Right > VPreparedMapRect.Right then begin
    VBitmapRect.Right := VPreparedMapRect.Right;
  end;
  VIntersectionAtPrepared := FPreparedConverter.MapRect2LocalRect(VBitmapRect, rrToTopLeft);
  VIntersectionAtBitmap := AConverter.MapRect2LocalRect(VBitmapRect, rrToTopLeft);
  for i := 0 to (VBitmapRect.Bottom - VBitmapRect.Top - 1) do begin
    if ATargetBitmap <> nil then begin
      VSourceLine := @ATargetBitmap.Data[VIntersectionAtBitmap.Left + i * ATargetBitmap.Size.X];
    end else begin
      VSourceLine := nil;
    end;
    PreparePixleLine(
      VSourceLine,
      Pointer(Cardinal(FPreparedData[i]) + Cardinal(VIntersectionAtPrepared.Left * FBytesPerPixel)),
      VBitmapRect.Right - VBitmapRect.Left
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
  Result := FLocalConverter.GetLocalRectSize;
end;

function TImageLineProviderAbstract.GetLine(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  ALine: Integer
): Pointer;
var
  VRect: TRect;
begin
  if (FPreparedConverter <> nil) then begin
    VRect := FPreparedConverter.GetLocalRect;
    if (ALine < VRect.Top) or (ALine >= VRect.Bottom) then begin
      FPreparedConverter := nil;
    end;
  end;

  if FPreparedConverter = nil then begin
    FPreparedConverter := PrepareConverterForLocalLine(ALine);
    PrepareBufferData(AOperationID, ACancelNotifier, FPreparedConverter);
  end;
  Result := GetLocalLine(ALine);
end;

function TImageLineProviderAbstract.GetLocalLine(ALine: Integer): Pointer;
var
  VPreparedLocalRect: TRect;
begin
  VPreparedLocalRect := FPreparedConverter.GetLocalRect;
  Assert(ALine >= VPreparedLocalRect.Top);
  Assert(ALine < VPreparedLocalRect.Bottom);
  Result := FPreparedData[ALine - VPreparedLocalRect.Top];
end;

procedure TImageLineProviderAbstract.PrepareBufferData(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AConverter: ILocalCoordConverter
);
var
  VTile: TPoint;
  VIterator: TTileIteratorByRectRecord;
  VTileConverter: ILocalCoordConverter;
  VRectOfTile: TRect;
begin
  PrepareBufferMem(AConverter.GetLocalRect);
  VRectOfTile := FMainGeoConverter.PixelRect2TileRect(AConverter.GetRectInMapPixel, FZoom);
  VIterator.Init(VRectOfTile);
  while VIterator.Next(VTile) do begin
      VTileConverter := FConverterFactory.CreateForTile(VTile, FZoom, FMainGeoConverter);
      AddTile(
        FImageProvider.GetBitmapRect(AOperationID, ACancelNotifier, VTileConverter),
        VTileConverter
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

function TImageLineProviderAbstract.PrepareConverterForLocalLine(
  ALine: Integer): ILocalCoordConverter;
var
  VPixel: TPoint;
  VTile: TPoint;
  VPreparedMapRect: TRect;
  VCurrentPieceRect: TRect;
  VCurrentPieceMapRect: TRect;
  VPreparedLocalRect: TRect;
  VPixelRect: TRect;
begin
  VCurrentPieceRect := FLocalConverter.GetLocalRect;
  VPixel :=
    FLocalConverter.LocalPixel2MapPixel(
      Types.Point(VCurrentPieceRect.Left, ALine),
      prToTopLeft
    );
  VTile := FMainGeoConverter.PixelPos2TilePos(VPixel, FZoom, prToTopLeft);
  VPixelRect := FMainGeoConverter.TilePos2PixelRect(VTile, FZoom);
  VCurrentPieceMapRect := FLocalConverter.GetRectInMapPixel;
  VPreparedMapRect :=
    Rect(
      VCurrentPieceMapRect.Left,
      VPixelRect.Top,
      VCurrentPieceMapRect.Right,
      VPixelRect.Bottom
    );

  VPreparedLocalRect := FLocalConverter.MapRect2LocalRect(VPreparedMapRect, rrToTopLeft);
  Result :=
    FConverterFactory.CreateConverterNoScale(
      VPreparedLocalRect,
      FZoom,
      FMainGeoConverter,
      VCurrentPieceMapRect.TopLeft
    );
end;

{ TImageLineProviderNoAlfa }

constructor TImageLineProviderNoAlfa.Create(
  const AImageProvider: IBitmapLayerProvider;
  const ALocalConverter: ILocalCoordConverter;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  ABgColor: TColor32
);
begin
  inherited Create(
    AImageProvider,
    ALocalConverter,
    AConverterFactory,
    ABgColor,
    3
  );
end;

{ TImageLineProviderWithAlfa }

constructor TImageLineProviderWithAlfa.Create(
  const AImageProvider: IBitmapLayerProvider;
  const ALocalConverter: ILocalCoordConverter;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  ABgColor: TColor32
);
begin
  inherited Create(
    AImageProvider,
    ALocalConverter,
    AConverterFactory,
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
