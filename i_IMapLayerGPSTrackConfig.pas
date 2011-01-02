unit i_IMapLayerGPSTrackConfig;

interface

type
  IMapLayerGPSTrackConfig = interface
    ['{5F9D5FD1-B40B-451A-B544-11C93A2B6532}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetLineWidth: Double;
    procedure SetLineWidth(AValue: Double);
    property LineWidth: Double read GetLineWidth write SetLineWidth;

    function GetLastPointCount: Integer;
    procedure SetLastPointCount(AValue: Integer);
    property LastPointCount: Integer read GetLastPointCount write SetLastPointCount;
  end;

implementation

end.
