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

    function GetReadyID: Integer;
    property ReadyID: Integer read GetReadyID;

    function GetExpectedID: Integer;
    property ExpectedID: Integer read GetExpectedID;

    function GetShownId: Integer;
    property ShownId: Integer read GetShownId;

    function CheckForShow: Boolean;

    procedure IncExpectedID;
    procedure UpdateBitmap(
      AID: Integer;
      const ABitmap: IBitmap32Static
    );

    function GetBitmap: IBitmap32Static;
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
