unit u_BitmapMapCombinerFactoryBase;

interface

uses
  Types,
  t_CommonTypes,
  t_MapCombineOptions,
  i_GeometryLonLat,
  i_BitmapMapCombiner,
  i_RegionProcessParamsFrame,
  u_BaseInterfacedObject;

type
  TBitmapMapCombinerFactoryBase = class(TBaseInterfacedObject, IBitmapMapCombinerFactory)
  private
    FMinPartSize: TPoint;
    FMaxPartSize: TPoint;
    FCombinePathStringTypeSupport: TStringTypeSupport;
    FDefaultExt: string;
    FFormatName: string;
    FOptionsSet: TMapCombineOptionsSet;
  private
    function GetMinPartSize: TPoint;
    function GetMaxPartSize: TPoint;
    function GetCombinePathStringTypeSupport: TStringTypeSupport;
    function GetDefaultExt: string;
    function GetFormatName: string;
    function GetOptionsSet: TMapCombineOptionsSet;
  protected
    function Validate(
      const AParams: IRegionProcessParamsFrameMapCombine;
      const APolygon: IGeometryLonLatPolygon
    ): Boolean; virtual;
    function PrepareMapCombiner(
      const AParams: IRegionProcessParamsFrameMapCombine;
      const AProgressInfo: IBitmapCombineProgressUpdate
    ): IBitmapMapCombiner; virtual; abstract;
  public
    constructor Create(
      const AMinPartSize: TPoint;
      const AMaxPartSize: TPoint;
      const ACombinePathStringTypeSupport: TStringTypeSupport;
      const ADefaultExt: string;
      const AFormatName: string;
      const AOptionsSet: TMapCombineOptionsSet = []
    );
  end;

implementation

{ TBitmapMapCombinerFactoryBase }

constructor TBitmapMapCombinerFactoryBase.Create(
  const AMinPartSize, AMaxPartSize: TPoint;
  const ACombinePathStringTypeSupport: TStringTypeSupport;
  const ADefaultExt, AFormatName: string;
  const AOptionsSet: TMapCombineOptionsSet
);
begin
  Assert(AMinPartSize.X <= AMaxPartSize.X);
  Assert(AMinPartSize.Y <= AMaxPartSize.Y);
  inherited Create;
  FMinPartSize := AMinPartSize;
  FMaxPartSize := AMaxPartSize;
  FCombinePathStringTypeSupport := ACombinePathStringTypeSupport;
  FDefaultExt := ADefaultExt;
  FFormatName := AFormatName;
  FOptionsSet := AOptionsSet;
end;

function TBitmapMapCombinerFactoryBase.GetCombinePathStringTypeSupport: TStringTypeSupport;
begin
  Result := FCombinePathStringTypeSupport;
end;

function TBitmapMapCombinerFactoryBase.GetDefaultExt: string;
begin
  Result := FDefaultExt;
end;

function TBitmapMapCombinerFactoryBase.GetFormatName: string;
begin
  Result := FFormatName;
end;

function TBitmapMapCombinerFactoryBase.GetMaxPartSize: TPoint;
begin
  Result := FMaxPartSize;
end;

function TBitmapMapCombinerFactoryBase.GetMinPartSize: TPoint;
begin
  Result := FMinPartSize;
end;

function TBitmapMapCombinerFactoryBase.GetOptionsSet: TMapCombineOptionsSet;
begin
  Result := FOptionsSet;
end;

function TBitmapMapCombinerFactoryBase.Validate(
  const AParams: IRegionProcessParamsFrameMapCombine;
  const APolygon: IGeometryLonLatPolygon
): Boolean;
begin
  Result := True;
end;

end.
