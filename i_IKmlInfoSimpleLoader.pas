unit i_IKmlInfoSimpleLoader;

interface

uses
  Classes,
  u_KmlInfoSimple;

type
  IKmlInfoSimpleLoader = interface
    ['{F9986E7D-897C-4BD3-8A92-A9798BFB32FA}']
    procedure LoadFromFile(AFileName: string; ABtm: TKmlInfoSimple);
    procedure LoadFromStream(AStream: TStream; ABtm: TKmlInfoSimple);
  end;

implementation

end.
 