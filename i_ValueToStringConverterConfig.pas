unit i_ValueToStringConverterConfig;

interface

uses
  t_CommonTypes,
  i_ConfigDataElement;

type
  IValueToStringConverterConfigStatic = interface
    ['{DFD404AC-DB7D-4108-9822-A0DD2943A5C7}']
    function GetDistStrFormat: TDistStrFormat;
    property DistStrFormat: TDistStrFormat read GetDistStrFormat;

    function GetIsLatitudeFirst: Boolean;
    property IsLatitudeFirst: Boolean read GetIsLatitudeFirst;

    function GetDegrShowFormat: TDegrShowFormat;
    property DegrShowFormat: TDegrShowFormat read GetDegrShowFormat;

    function GetAreaShowFormat: TAreaStrFormat;
    property AreaShowFormat: TAreaStrFormat read GetAreaShowFormat;
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

    function GetAreaShowFormat: TAreaStrFormat;
    procedure SetAreaShowFormat(AValue: TAreaStrFormat);
    property AreaShowFormat: TAreaStrFormat read GetAreaShowFormat write SetAreaShowFormat;

    function GetStatic: IValueToStringConverterConfigStatic;
  end;

implementation

end.
