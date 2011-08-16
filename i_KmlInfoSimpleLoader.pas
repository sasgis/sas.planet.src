unit i_KmlInfoSimpleLoader;

interface

uses
  Classes,
  i_VectorDataItemSimple;

type
  IVectorDataLoader = interface
    ['{F9986E7D-897C-4BD3-8A92-A9798BFB32FA}']
    procedure LoadFromFile(AFileName: string; out AItems: IVectorDataItemList);
    procedure LoadFromStream(AStream: TStream;  out AItems: IVectorDataItemList);
  end;

implementation

end.
 