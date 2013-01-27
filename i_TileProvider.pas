unit i_TileProvider;

interface

uses
  Types,
  i_NotifierTilePyramidUpdate,
  i_ProjectionInfo,
  i_VectorDataItemSimple,
  i_Bitmap32Static;

type
  IBitmapTileProvider = interface
    ['{88ACB3F9-FDEE-4451-89A0-EA24133E2DB5}']
    function GetProjectionInfo: IProjectionInfo;
    property ProjectionInfo: IProjectionInfo read GetProjectionInfo;

    function GetTile(
      const ATile: TPoint
    ): IBitmap32Static;
  end;

  IBitmapTileProviderWithNotifier = interface(IBitmapTileProvider)
    ['{DB94FB95-B32E-434C-8DF9-0647BE84052D}']
    function GetChangeNotifier: INotifierTilePyramidUpdate;
    property ChangeNotifier: INotifierTilePyramidUpdate read GetChangeNotifier;
  end;

  IVectorTileProvider = interface
    ['{00ADB9F4-D421-4F71-A9B6-3F8A6E8FFCB9}']
    function GetProjectionInfo: IProjectionInfo;
    property ProjectionInfo: IProjectionInfo read GetProjectionInfo;

    function GetTile(
      const ATile: TPoint
    ): IVectorDataItemList;
  end;

  IVectorTileProviderWithNotifier = interface(IVectorTileProvider)
    ['{3C1CF0A0-02F2-4F41-85FA-1F108BB0F120}']
    function GetChangeNotifier: INotifierTilePyramidUpdate;
    property ChangeNotifier: INotifierTilePyramidUpdate read GetChangeNotifier;
  end;

implementation

end.
