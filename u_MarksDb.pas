unit u_MarksDb;

interface

uses
  ActiveX,
  t_GeoTypes,
  i_Marks;

type
  TMarksDb = class(TInterfacedObject, IMarksDb)
  private

  public
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
    function GetMarksVisibleInRect(ALonLat: TExtendedRect): IEnumUnknown;
  end;

implementation

type
  TEmptyEnum = class(TInterfacedObject, IEnumUnknown)
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumUnknown): HResult; stdcall;
  end;

{ TMarksDb }

function TMarksDb.AddCategory(ACategory: IMarkCategory): integer;
begin

end;

function TMarksDb.AddMark(AMark: IMarkBasic): integer;
begin

end;

procedure TMarksDb.DeleteCategory(IdCategory: integer);
begin

end;

procedure TMarksDb.DeleteMark(IdMark: integer);
begin

end;

function TMarksDb.GetAllCategories: IEnumUnknown;
begin

end;

function TMarksDb.GetAllMarksOfCategory(
  ACategoryId: integer): IEnumUnknown;
begin

end;

function TMarksDb.GetCategory(IdCategory: integer): IMarkCategory;
begin

end;

function TMarksDb.GetMark(IdMark: integer): IMarkBasic;
begin

end;

function TMarksDb.GetMarksVisibleInRect(
  ALonLat: TExtendedRect): IEnumUnknown;
begin

end;

function TMarksDb.GetVisibleCategories: IEnumUnknown;
begin

end;

procedure TMarksDb.ReplaceCategory(IdCategory: integer;
  ACategory: IMarkCategory);
begin

end;

procedure TMarksDb.ReplaceMark(IdMark: integer; AMark: IMarkBasic);
begin

end;

{ TEmptyEnum }

function TEmptyEnum.Clone(out enm: IEnumUnknown): HResult;
begin
  enm := Self;
  Result := S_OK;
end;

function TEmptyEnum.Next(celt: Integer; out elt;
  pceltFetched: PLongint): HResult;
begin
  pceltFetched^ := 0;
  Result := S_FALSE;
end;

function TEmptyEnum.Reset: HResult;
begin
  Result := S_OK;
end;

function TEmptyEnum.Skip(celt: Integer): HResult;
begin
  Result := S_FALSE;
end;

end.
