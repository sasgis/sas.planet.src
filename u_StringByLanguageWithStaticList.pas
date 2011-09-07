unit u_StringByLanguageWithStaticList;

interface

uses
  Classes,
  i_LanguageListStatic,
  i_StringByLanguage;

type
  TStringByLanguageWithStaticList = class(TInterfacedObject, IStringByLanguage)
  private
    FValueList: TStringList;
  protected
    function GetString(ALangIndex: Integer): string;
    function GetDefault: string;
  public
    constructor Create(
      AValueList: TStrings
    );
    destructor Destroy; override;
  end;
implementation

uses
  SysUtils;

{ TStringByLangByStaticList }

constructor TStringByLanguageWithStaticList.Create(AValueList: TStrings);
begin
  FValueList := TStringList.Create;
  Assert(AValueList.Count > 0);
  FValueList.Assign(AValueList);
end;

destructor TStringByLanguageWithStaticList.Destroy;
begin
  FreeAndNil(FValueList);
  inherited;
end;

function TStringByLanguageWithStaticList.GetDefault: string;
begin
  Result := FValueList.Strings[0];
end;

function TStringByLanguageWithStaticList.GetString(ALangIndex: Integer): string;
begin
  if (ALangIndex > 0) and (ALangIndex < FValueList.Count) then begin
    Result := FValueList.Strings[ALangIndex];
  end else begin
    Result := GetDefault;
  end;
end;

end.
