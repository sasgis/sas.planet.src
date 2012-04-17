unit u_TileMatrixElement;

interface

uses
  Types,
  SysUtils,
  i_Bitmap32Static,
  i_LocalCoordConverter,
  i_TileMatrix;

type
  TTileMatrixElement = class(TInterfacedObject, ITileMatrixElement)
  private
    FSync: IReadWriteSync;

    FTile: TPoint;
    FLocalConverter: ILocalCoordConverter;

    FIsReady: Boolean;
    FBitmap: IBitmap32Static;
  private
    function GetTile: TPoint;
    function GetLocalConverter: ILocalCoordConverter;

    function GetIsReady: Boolean;
    procedure SetIsReady(AValue: Boolean);

    function GetBitmap: IBitmap32Static;
    procedure SetBitmap(const AValue: IBitmap32Static);
  public
    constructor Create(
      const ATile: TPoint;
      const ALocalConverter: ILocalCoordConverter;
      const ABitmap: IBitmap32Static
    );
  end;

implementation

uses
  i_CoordConverter,
  u_Synchronizer;

{ TTileMatrixElement }

constructor TTileMatrixElement.Create(
  const ATile: TPoint;
  const ALocalConverter: ILocalCoordConverter;
  const ABitmap: IBitmap32Static
);
var
  VZoom: Byte;
  VConverter: ICoordConverter;
  VMapPixelRect: TRect;
begin
  inherited Create;
  FTile := ATile;
  FLocalConverter := ALocalConverter;
  FBitmap := ABitmap;
  FSync := MakeSyncRW_Var(Self);
  FIsReady := False;
  VZoom := FLocalConverter.Zoom;
  VConverter := FLocalConverter.GeoConverter;
  VMapPixelRect := FLocalConverter.GetRectInMapPixel;
  Assert(EqualRect(VMapPixelRect, VConverter.TilePos2PixelRect(FTile, VZoom)));
end;

function TTileMatrixElement.GetBitmap: IBitmap32Static;
begin
  FSync.BeginRead;
  try
    Result := FBitmap;
  finally
    FSync.EndRead;
  end;
end;

function TTileMatrixElement.GetIsReady: Boolean;
begin
  FSync.BeginRead;
  try
    Result := FIsReady;
  finally
    FSync.EndRead;
  end;
end;

function TTileMatrixElement.GetLocalConverter: ILocalCoordConverter;
begin
  Result := FLocalConverter;
end;

function TTileMatrixElement.GetTile: TPoint;
begin
  Result := FTile;
end;

procedure TTileMatrixElement.SetBitmap(const AValue: IBitmap32Static);
begin
  FSync.BeginWrite;
  try
    FBitmap := AValue;
  finally
    FSync.EndWrite;
  end;
end;

procedure TTileMatrixElement.SetIsReady(AValue: Boolean);
begin
  FSync.BeginWrite;
  try
    FIsReady := AValue;
  finally
    FSync.EndWrite;
  end;
end;

end.
