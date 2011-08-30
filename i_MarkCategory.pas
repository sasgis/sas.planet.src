unit i_MarkCategory;

interface

type
  ICategory = interface
    ['{B870BAEC-8ADD-4D29-9A9E-B9131C0C5681}']
    function GetName: string; stdcall;
    property Name: string read GetName;

    function IsSame(ACategory: ICategory): Boolean;
  end;

  IMarkCategory = interface(ICategory)
  ['{00226B68-9915-41AA-90B7-3F2348E53527}']
    function GetVisible: boolean; stdcall;
    property Visible: boolean read GetVisible;

    function GetAfterScale: integer; stdcall;
    property AfterScale: integer read GetAfterScale;

    function GetBeforeScale: integer; stdcall;
    property BeforeScale: integer read GetBeforeScale;

    function IsNew: Boolean;
  end;

implementation

end.
