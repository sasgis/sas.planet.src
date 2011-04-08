unit i_MarksDbSmlInternal;

interface

type
  IMarkSMLInternal = interface
  ['{2611AAA5-10DA-472B-B3EE-31EA27EDD6CD}']
    function GetDbCode: Integer;
    property DbCode: Integer read GetDbCode;

    function GetId: Integer;
    property Id: Integer read GetId;

    function GetCategoryId: Integer;
    property CategoryId: Integer read GetCategoryId;

    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  IMarkPointSMLInternal = interface(IMarkSMLInternal)
  ['{2611AAA5-10DA-472B-B3EE-31EA27EDD6CD}']
    function GetPicName: string;
    property PicName: string read GetPicName;
  end;

  IMarkCategorySMLInternal = interface
    ['{28CEA1EA-F64D-4CD6-81CC-4ACAEAECD4B1}']
    function GetId: integer; stdcall;
    property Id: integer read GetId;
  end;

  IMarkTemplateSMLInternal = interface
    ['{8941E424-A7EF-4EB7-A2CD-6754107D5AE5}']
    function GetCategoryId: Integer;
    property CategoryId: Integer read GetCategoryId;
  end;

implementation

end.
