unit u_KmlInfoSimple;

interface

uses
  t_GeoTypes;

type
  TKMLData = record
    PlacemarkID: string;
    Name: string;
    description: string;
    coordinates: TExtendedPointArray;
    coordinatesLT: TExtendedPoint;
    coordinatesRD: TExtendedPoint;
  end;

  TKmlInfoSimple = class
  public
    Data: Array of TKMLData;
    constructor Create;
    destructor Destroy; override;
  end;



implementation

{ TKmlInfoSimple }

constructor TKmlInfoSimple.Create;
begin
  Data := nil;
end;

destructor TKmlInfoSimple.Destroy;
var
  i: integer;
begin
  if Data <> nil then begin
    for i := 0 to Length(Data) - 1 do begin
      Data[i].PlacemarkID := '';
      Data[i].Name := '';
      Data[i].description := '';
      Data[i].coordinates := nil;
    end;
  end;
  inherited;
end;

end.
 