{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ImageTileProvider;

interface

uses
  Types,
  t_Bitmap32,
  i_Bitmap32Static,
  i_BitmapTileProvider,
  i_ImageTileProvider,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  u_BaseInterfacedObject;

type
  TImageTileProviderAbstract = class(TBaseInterfacedObject, IImageTileProvider)
  private
    FTileSize: TPoint;

    FBuffer: Pointer;
    FBufferSize: Integer;

    FGetTileCounter: IInternalPerformanceCounter;
    FImageProvider: IBitmapTileProvider;
    FBytesPerPixel: Integer;
  protected
    procedure PrepareTile(
      ASource: PColor32;
      ATarget: Pointer;
      ACount: Integer
    ); virtual; abstract;
  private
    { IImageTileProvider }
    function GetTileSize: TPoint;
    function GetBytesPerPixel: Integer;

    function GetTile(
      const AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const APoint: TPoint;
      const AZoom: Integer
    ): Pointer;
  public
    constructor Create(
      const AGetTileCounter: IInternalPerformanceCounter;
      const AImageProvider: IBitmapTileProvider;
      const ABytesPerPixel: Integer
    );
    destructor Destroy; override;
  end;

  TImageTileProviderRGB = class(TImageTileProviderAbstract)
  protected
    procedure PrepareTile(
      ASource: PColor32;
      ATarget: Pointer;
      ACount: Integer
    ); override;
  public
    constructor Create(
      const AGetTileCounter: IInternalPerformanceCounter;
      const AImageProvider: IBitmapTileProvider
    );
  end;

  TImageTileProviderRGBA = class(TImageTileProviderAbstract)
  protected
    procedure PrepareTile(
      ASource: PColor32;
      ATarget: Pointer;
      ACount: Integer
    ); override;
  public
    constructor Create(
      const AGetTileCounter: IInternalPerformanceCounter;
      const AImageProvider: IBitmapTileProvider
    );
  end;

implementation

uses
  SysUtils,
  c_CoordConverter;

{ TImageTileProviderAbstract }

constructor TImageTileProviderAbstract.Create(
  const AGetTileCounter: IInternalPerformanceCounter;
  const AImageProvider: IBitmapTileProvider;
  const ABytesPerPixel: Integer
);
var
  VTileSplitCode: Integer;
begin
  inherited Create;

  FGetTileCounter := AGetTileCounter;
  FImageProvider := AImageProvider;
  FBytesPerPixel := ABytesPerPixel;

  VTileSplitCode := FImageProvider.Projection.GetTileSplitCode;
  case VTileSplitCode of
    CTileSplitQuadrate256x256: begin
      FTileSize := Types.Point(256, 256);
    end;
  else
    raise Exception.CreateFmt('Unexpected TileSplitCode: %d', [VTileSplitCode]);
  end;

  FBufferSize := FTileSize.X * FTileSize.Y * FBytesPerPixel;
  GetMem(FBuffer, FBufferSize);
end;

destructor TImageTileProviderAbstract.Destroy;
begin
  FreeMem(FBuffer);
  inherited Destroy;
end;

function TImageTileProviderAbstract.GetTileSize: TPoint;
begin
  Result := FTileSize;
end;

function TImageTileProviderAbstract.GetBytesPerPixel: Integer;
begin
  Result := FBytesPerPixel;
end;

function TImageTileProviderAbstract.GetTile(
  const AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const APoint: TPoint;
  const AZoom: Integer
): Pointer;
var
  VBitmap: IBitmap32Static;
  VContext: TInternalPerformanceCounterContext;
begin
  Result := nil;

  // todo: add overview support

  VContext := FGetTileCounter.StartOperation;
  try
    VBitmap := FImageProvider.GetTile(AOperationID, ACancelNotifier, APoint);
    if VBitmap <> nil then begin
      PrepareTile(@VBitmap.Data[0], FBuffer, FBufferSize div FBytesPerPixel);
      Result := FBuffer;
    end;
  finally
    FGetTileCounter.FinishOperation(VContext);
  end;
end;

{ TImageTileProviderRGB }

constructor TImageTileProviderRGB.Create(
  const AGetTileCounter: IInternalPerformanceCounter;
  const AImageProvider: IBitmapTileProvider
);
begin
  inherited Create(AGetTileCounter, AImageProvider, 3);
end;

procedure TImageTileProviderRGB.PrepareTile(ASource: PColor32; ATarget: Pointer; ACount: Integer);
type
  TRGB  = packed record R,G,B: Byte; end;
var
  I: Integer;
  VSource: PColor32Entry absolute ASource;
  VTarget: ^TRGB absolute ATarget;
begin
  for I := 0 to ACount - 1 do begin
    VTarget.B := VSource.B;
    VTarget.G := VSource.G;
    VTarget.R := VSource.R;
    Inc(VSource);
    Inc(VTarget);
  end;
end;

{ TImageTileProviderRGBA }

constructor TImageTileProviderRGBA.Create(
  const AGetTileCounter: IInternalPerformanceCounter;
  const AImageProvider: IBitmapTileProvider
);
begin
  inherited Create(AGetTileCounter, AImageProvider, 4);
end;

procedure TImageTileProviderRGBA.PrepareTile(ASource: PColor32; ATarget: Pointer; ACount: Integer);
type
  TRGBA = packed record R,G,B,A: Byte; end;
var
  I: Integer;
  VSource: PColor32Entry absolute ASource;
  VTarget: ^TRGBA absolute ATarget;
begin
  for I := 0 to ACount - 1 do begin
    VTarget.B := VSource.B;
    VTarget.G := VSource.G;
    VTarget.R := VSource.R;
    VTarget.A := VSource.A;
    Inc(VSource);
    Inc(VTarget);
  end;
end;

end.
