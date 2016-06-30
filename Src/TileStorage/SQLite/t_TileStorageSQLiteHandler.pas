unit t_TileStorageSQLiteHandler;

interface

uses
  t_TileStorageSQLite,
  t_NotifierOperationRec,
  i_MapVersionInfo,
  i_InterfaceListSimple;

type
  TVersionColMode = (
    vcm_None = 0,
    vcm_Int,
    vcm_Text
  );

  TTBColInfo = record
    // 5..8: x, y, [v], [c], s, [h], d, b
    ColCount: Integer;

    // 0 - no field 'v' for Version
    // 1 - has field 'v' of type INT (actually Int64)
    // 2 - has field 'v' of type TEXT
    ModeV: TVersionColMode;

    // if true - has field 'c' for content-type as TEXT
    HasC: Boolean;

    // if true - has field 'h' for crc32 as INT
    HasH: Boolean;
  end;
  PTBColInfo = ^TTBColInfo;

  TSelectTileInfoComplex = record
    TileResult: PGetTileResult;
    RequestedVersionInfo: IMapVersionInfo; // запрошенна€ верси€ (как заготовка дл€ читаемого тайла)
    RequestedVersionAsInt: Int64;          // результат конвертации  требуемой версии в Int64
    RequestedVersionIsInt: Boolean;        // признак, что требуема€ верси€ целочисленна€
    RequestedVersionIsSet: Boolean;        // признак, что требуема€ верси€ установлена (не пуста€)
    RequestedVersionToDB: AnsiString;      // верси€ дл€ запроса к Ѕƒ
    SelectMode: TGetTileInfoModeSQLite;
  end;
  PSelectTileInfoComplex = ^TSelectTileInfoComplex;

implementation


end.
