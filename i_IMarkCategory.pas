unit i_IMarkCategory;

interface

type
  IMarkCategory = interface
  ['{00226B68-9915-41AA-90B7-3F2348E53527}']
    function GetId: integer; stdcall;
    property Id: integer read GetId;

    function GetName: string; stdcall;
    property Name: string read GetName;

    function GetVisible: boolean; stdcall;
    property Visible: boolean read GetVisible;

    function GetAfterScale: integer; stdcall;
    property AfterScale: integer read GetAfterScale;

    function GetBeforeScale: integer; stdcall;
    property BeforeScale: integer read GetBeforeScale;
  end;

implementation

end.
