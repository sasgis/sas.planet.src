unit i_VectorDataItemSimple;

interface

uses
  t_GeoTypes;

type
  IVectorDataItemSimple =  interface
    ['{1242B43D-C878-4AC9-9F29-0A3E258F4670}']
    function GetName: string;
    property Name: string read GetName;

    function GetDesc: string;
    property Desc: string read GetDesc;

    function GetLLRect: TDoubleRect;
    property LLRect: TDoubleRect read GetLLRect;

    function GetPoints: TArrayOfDoublePoint;
    property Points: TArrayOfDoublePoint read GetPoints;

    function GetHintText: string;
    function GetInfoHTML: string;
    function GetInfoCaption: string;

    function IsPoint: Boolean;
    function IsLine: Boolean;
    function IsPoly: Boolean;
  end;

implementation

end.
