unit i_MarkerRingsConfig;

interface

uses
  i_ConfigDataElement;

type
  IMarkerRingsConfigStatic = interface
    ['{7B1A1CBB-7E9B-47F6-B657-6E800FABC7B3}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetStepDistance: Double;
    property StepDistance: Double read GetStepDistance;
  end;

  IMarkerRingsConfig = interface(IConfigDataElement)
    ['{56C28B06-2AC9-46B9-9E7E-CBEC3A41B975}']
    function GetCount: Integer;
    procedure SetCount(AValue: Integer);
    property Count: Integer read GetCount write SetCount;

    function GetStepDistance: Double;
    procedure SetStepDistance(AValue: Double);
    property StepDistance: Double read GetStepDistance write SetStepDistance;

    function GetStatic: IMarkerRingsConfigStatic;
  end;

implementation

end.
