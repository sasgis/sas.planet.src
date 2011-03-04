unit i_Marks;

interface

uses
  ActiveX,
  t_GeoTypes;


type
  IMarkSimple = interface
    function GetRect: TDoubleRect; safecall;
    function GetName: WideString; safecall;
    function GetDescr: WideString; safecall;
  end;
  
  IMarkPoint = interface(IMarkSimple)
    function GetPicName: WideString; safecall;
  end;

  IEnumDoublePoint = interface
    // Методы для как у стандартных IEnumXXXX

    //Сброс итератора.
    procedure Reset(); safecall;

    //Пропустить заданное количество тайлов.
    //Возвращает S_OK если получилось пропустьить именно столько сколько требовалось.
    function Skip(celt: Cardinal): HRESULT; stdcall;

    //Получить масив из заданного в celt количества тайлов. rgelt указатель на возвращаемый массив,
    //celtFetched количество реально полученных элементов. Возвращает S_OK если celt=celtFetched
    //память под массив должна быть выделена клиентом в количестве достаточном для хранения запрошенного
    //количества элементов
    function Next(celt: Cardinal; out rgelt; var celtFetched: Cardinal): HRESULT; stdcall;

    // Делает копию текущего итератора с тем же состоянием. Итератор может не поддерживать возможность
    // и может вернуть E_NOTIMPL
    function Clone(out penum: IEnumDoublePoint): HRESULT; stdcall;
  end;

  IDoublePointArray = interface
    function GetCount: Cardinal; safecall;
    function GetPoint(AIndex: Cardinal): TDoublePoint; safecall;
    function GetEnum: IEnumDoublePoint; safecall;
  end;

  IMarkPath = interface(IMarkSimple)
    function GetColor: Cardinal; stdcall;
    function GetLineWidth: Cardinal; stdcall;
    function GetPoints: IDoublePointArray;
  end;

  IMarkPoly = interface(IMarkSimple)
    function GetBorderColor: Cardinal; stdcall;
    function GetFillColor: Cardinal; stdcall;
    function GetPoints: IDoublePointArray;
  end;



  IMarkBasic = interface
  ['{CCB5B32C-F6F9-4445-B0EF-D14C19C8D761}']
    function GetId: integer; stdcall;
    function GetCategoryId: integer; stdcall;
    function GetColor1: integer; stdcall;
    function GetColor2: integer; stdcall;
    function GetDescr: string; stdcall;
    function GetLonLatArr: TArrayOfDoublePoint; stdcall;
    function GetName: string; stdcall;
    function GetPicName: string; stdcall;
    function GetRect: TDoubleRect; stdcall;
    function GetScale1: integer; stdcall;
    function GetScale2: integer; stdcall;
    function GetVisible: boolean; stdcall;

    property Id: integer read GetId;
    property Name: string read GetName;
    property Descr: string read GetDescr;
    property Scale1: integer read GetScale1;
    property Scale2: integer read GetScale2;
    property LonLatArr: TArrayOfDoublePoint read GetLonLatArr;
    property Rect: TDoubleRect read GetRect;
    property Color1: integer read GetColor1;
    property Color2: integer read GetColor2;
    property Visible: boolean read GetVisible;
    property PicName: string read GetPicName;
    property CategoryId: integer read GetCategoryId;
  end;

  IMarkCategory = interface
  ['{00226B68-9915-41AA-90B7-3F2348E53527}']
    function GetId: integer; stdcall;
    function GetName: string; stdcall;
    function GetVisible: boolean; stdcall;
    function GetAfterScale: integer; stdcall;
    function GetBeforeScale: integer; stdcall;

    property Id: integer read GetId;
    property Name: string read GetName;
    property Visible: boolean read GetVisible;
    property AfterScale: integer read GetAfterScale;
    property BeforeScale: integer read GetBeforeScale;
  end;

  IMarksDb = interface
  ['{ADAC52ED-3FB2-4D87-950A-595EF2073C03}']
    function AddMark(AMark: IMarkBasic): integer;
    function GetMark(IdMark: integer): IMarkBasic;
    procedure DeleteMark(IdMark: integer);
    procedure ReplaceMark(IdMark: integer; AMark: IMarkBasic);

    function AddCategory(ACategory: IMarkCategory): integer;
    function GetCategory(IdCategory: integer): IMarkCategory;
    procedure DeleteCategory(IdCategory: integer);
    procedure ReplaceCategory(IdCategory: integer; ACategory: IMarkCategory);

    function GetAllCategories: IEnumUnknown;
    function GetVisibleCategories: IEnumUnknown;
    function GetAllMarksOfCategory(ACategoryId: integer): IEnumUnknown;
    function GetMarksVisibleInRect(ALonLat: TDoubleRect): IEnumUnknown;
  end;
implementation


end.
