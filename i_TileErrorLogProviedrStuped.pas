unit i_TileErrorLogProviedrStuped;

interface

uses
  i_JclNotify,
  i_TileError;

type
  ITileErrorLogProviedrStuped = interface
    ['{07A873B2-D60A-4840-AA6F-97F216B2FE23}']
    function GetLastErrorInfo: ITileErrorInfo;
    function GetNotifier: IJclNotifier;
  end;

implementation

end.
