unit i_TileMatrix;

interface

uses
  Types,
  i_Bitmap32Static,
  i_LocalCoordConverter;

type
  ITileMatrixElement = interface
    ['{3F060F92-8F09-4989-AFD8-B7D2B8E5DC20}']
    function GetTile: TPoint;
    property Tile: TPoint read GetTile;

    function GetLocalConverter: ILocalCoordConverter;
    property LocalConverter: ILocalCoordConverter read GetLocalConverter;

    function GetIsReady: Boolean;
    procedure SetIsReady(AValue: Boolean);
    property IsReady: Boolean read GetIsReady write SetIsReady;

    function GetBitmap: IBitmap32Static;
    procedure SetBitmap(AValue: IBitmap32Static);
    property Bitmap: IBitmap32Static read GetBitmap write SetBitmap;
  end;

  ITileMatrix = interface
    ['{ABC6376A-F0CD-408F-8F19-043633E6D374}']
    function GetLocalConverter: ILocalCoordConverter;
    property LocalConverter: ILocalCoordConverter read GetLocalConverter;

    function GetTileRect: TRect;
    property TileRect: TRect read GetTileRect;

    function GetElementByTile(const ATile: TPoint): ITileMatrixElement;

    function GetItem(AX, AY: Integer): ITileMatrixElement;
    property Items[AX, AY: Integer]: ITileMatrixElement read GetItem;
  end;

  ITileMatrixFactory = interface
    ['{63A336D2-751D-4C4F-BC18-CF3791206619}']
    function BuildNewMatrix(
      const ASource: ITileMatrix;
      const ANewConverter: ILocalCoordConverter
    ): ITileMatrix;
  end;

implementation

end.
