unit t_TileStorageSQLite;

interface

uses
  Types,
  i_BinaryData,
  i_TileInfoBasic,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  u_TileRectInfoShort; // for TArrayOfTileInfoShortInternal

type
  TGetTileInfoItem = (
    gtiiLoadDate,
    gtiiSize,
    gtiiBody,
    gtiiContentType
  );
  TGetTileInfoModeSQLite = set of TGetTileInfoItem;

  TSaveTileFlags = Byte;
  TDeleteTileFlags = Byte;
  TReplaceVersionFlags = Byte;

  TDeleteTileAllData = record
    DXY: TPoint;
    DZoom: Byte;
    DVersionInfo: IMapVersionInfo;
    DDeleteTileFlags: TDeleteTileFlags;
    DPrevSizeValue: Integer;
  end;
  PDeleteTileAllData = ^TDeleteTileAllData;

  TSaveTileAllData = record
    SXY: TPoint;
    SZoom: Byte;
    SVersionInfo: IMapVersionInfo;
    SLoadDate: TDateTime;
    SContentType: IContentTypeInfoBasic;
    SData: IBinaryData;
    SSaveTileFlags: TSaveTileFlags;
  end;
  PSaveTileAllData = ^TSaveTileAllData;

  TSetTileVersionAllData = record
    SXY: TPoint;
    SZoom: Byte;
    SVersionSrc: IMapVersionInfo;
    SVersionDst: IMapVersionInfo;
    SReplaceVersionFlags: TReplaceVersionFlags;
  end;
  PSetTileVersionAllData = ^TSetTileVersionAllData;

  TGetTileInfo = record
    GTilePos: TPoint;
    GZoom: Byte;
    GVersion: IMapVersionInfo;
    GShowPrevVersion: Boolean;
    GMode: TGetTileInfoModeSQLite;
  end;
  PGetTileInfo = ^TGetTileInfo;

  TTileInfoShortEnumData = record
    DestRect: TRect;
    DestZoom: Byte;
    RectVersionInfo: IMapVersionInfo;
    RectCount: TPoint;
    RectItems: TArrayOfTileInfoShortInternal;
  end;
  PTileInfoShortEnumData = ^TTileInfoShortEnumData;

  TGetTileResult = record
    // результат
    GTileInfo: ITileInfoBasic;
    // дополнительные параметры, которые прочитаны, но которые возможно не запрашивались
    GExtraMode: TGetTileInfoModeSQLite;
  end;
  PGetTileResult = ^TGetTileResult;

implementation

end.
