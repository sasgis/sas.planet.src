unit i_GlobalCacheConfig;

interface

uses
  i_PathConfig,
  i_ConfigDataElement;

type
  IGlobalCacheConfig = interface(IConfigDataElement)
    ['{DAE1C885-4548-4DD5-AC19-57825DFBECF6}']
    //Способ храения кэша по-умолчанию.
    function GetDefCache: byte;
    procedure SetDefCache(const AValue: byte);
    property DefCache: byte read GetDefCache write SetDefCache;

    //Пути к кэшам разных типов
    function GetNewCPath: IPathConfig;
    property NewCPath: IPathConfig read GetNewCPath;
    function GetOldCPath: IPathConfig;
    property OldCPath: IPathConfig read GetOldCPath;
    function GetESCPath: IPathConfig;
    property ESCPath: IPathConfig read GetESCPath;
    function GetGMTilesPath: IPathConfig;
    property GMTilesPath: IPathConfig read GetGMTilesPath;
    function GetGECachePath: IPathConfig;
    property GECachePath: IPathConfig read GetGECachePath;
    function GetGCCachePath: IPathConfig;
    property GCCachePath: IPathConfig read GetGCCachePath;
    function GetBDBCachePath: IPathConfig;
    property BDBCachePath: IPathConfig read GetBDBCachePath;
    function GetDBMSCachePath: IPathConfig;
    property DBMSCachePath: IPathConfig read GetDBMSCachePath;
  end;

implementation

end.
