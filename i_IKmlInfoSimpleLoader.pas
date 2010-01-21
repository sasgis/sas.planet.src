unit i_IKmlInfoSimpleLoader;

interface

uses
  Classes,
  u_KmlInfoSimple;

type
  IKmlInfoSimpleLoader = interface
  ['{07D84005-DD59-4750-BCCE-A02330734539}']
    procedure LoadFromFile(AFileName: string; ABtm: TKmlInfoSimple);
    procedure LoadFromStream(AStream: TStream; ABtm: TKmlInfoSimple);
  end;

implementation

end.
 