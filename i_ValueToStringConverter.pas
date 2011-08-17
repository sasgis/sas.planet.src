unit i_ValueToStringConverter;

interface

uses
  t_GeoTypes,
  t_CommonTypes,
  i_ConfigDataElement;

type
  IValueToStringConverter = interface
    ['{9EC20437-48BD-4D18-BF95-D2390C6F26F5}']
    function DataSizeConvert(ASizeInKb: Double): string;
    function DistConvert(ADistInMeters: Double): string;
    function DistPerPixelConvert(ADistPerPixelInMeters: Double): string;
    function AreaConvert(AAreaInSqm: Double): string;
    function SpeedConvert(AKmph: Double): string;
    function AltitudeConvert(AMeters: Double): string;
    function LonLatConvert(ALonLat: TDoublePoint): string;
    function LonConvert(ALon: Double): string;
    function LatConvert(ALat: Double): string;
  end;

  IValueToStringConverterConfig = interface(IConfigDataElement)
    ['{DDC4DF45-A387-43DC-AED7-33935241C718}']
    function GetDistStrFormat: TDistStrFormat;
    procedure SetDistStrFormat(AValue: TDistStrFormat);
    property DistStrFormat: TDistStrFormat read GetDistStrFormat write SetDistStrFormat;

    function GetIsLatitudeFirst: Boolean;
    procedure SetIsLatitudeFirst(AValue: Boolean);
    property IsLatitudeFirst: Boolean read GetIsLatitudeFirst write SetIsLatitudeFirst;

    function GetDegrShowFormat: TDegrShowFormat;
    procedure SetDegrShowFormat(AValue: TDegrShowFormat);
    property DegrShowFormat: TDegrShowFormat read GetDegrShowFormat write SetDegrShowFormat;

    function GetStatic: IValueToStringConverter;
  end;

implementation

end.
