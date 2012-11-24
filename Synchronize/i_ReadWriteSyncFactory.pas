unit i_ReadWriteSyncFactory;

interface

uses
  SysUtils;

type
  IReadWriteSyncFactory = interface
    ['{EA854C25-64EF-456B-B125-D2456223898F}']
    function Make(const AName: AnsiString): IReadWriteSync;
  end;

implementation

end.
