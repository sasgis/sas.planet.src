unit i_PointOnMapEdit;

interface

uses
  t_GeoTypes,
  i_ConfigDataElement;

type
  IPointOnMapEdit = interface(IConfigDataElement)
    ['{EAC4A38A-50A4-4FAC-94E4-1F5493F70376}']
    function GetPoint: TDoublePoint;
    procedure SetPoint(const AValue: TDoublePoint);
    property Point: TDoublePoint read GetPoint write SetPoint;

    procedure Clear;
  end;

implementation

end.
