unit i_INavigationToPoint;

interface

uses
  t_GeoTypes,
  i_ConfigDataElement;

type
  INavigationToPoint = interface(IConfigDataElement)
    ['{61EBB721-A3D9-402B-ACDC-FF3E2DA5C262}']
    function GetIsActive: Boolean;
    property IsActive: Boolean read GetIsActive;

    function GetId: Integer;
    property Id: Integer read GetId;

    function GetLonLat: TDoublePoint;
    property LonLat: TDoublePoint read GetLonLat;

    procedure StartNavToMark(AId: Integer; APointLonLat: TDoublePoint);
    procedure StartNavLonLat(APointLonLat: TDoublePoint);
    procedure StopNav;
  end;
implementation

end.
