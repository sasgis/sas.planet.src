unit i_IMarkCategoryFactoryConfig;

interface

uses
  i_IConfigDataElement;

type
  IMarkCategoryFactoryConfig = interface(IConfigDataElement)
    ['{5F3F4E37-FF16-4DC6-8A37-ADB30868C2DA}']
    function GetDefaultName: string;
    procedure SetDefaultName(AValue: string);
    property DefaultName: string read GetDefaultName write SetDefaultName;

    function GetAfterScale: Integer;
    procedure SetAfterScale(AValue: Integer);
    property AfterScale: Integer read GetAfterScale write SetAfterScale;

    function GetBeforeScale: Integer;
    procedure SetBeforeScale(AValue: Integer);
    property BeforeScale: Integer read GetBeforeScale write SetBeforeScale;
  end;

implementation

end.
