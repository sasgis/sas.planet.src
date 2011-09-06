unit i_LanguageListStatic;

interface

type
  ILanguageListStatic = interface
    ['{EE282D06-5C24-4A10-A3DD-610A2ADEB515}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetCode(AIndex: Integer): string;
    function FindCode(ACode: string; out AIndex: Integer): Boolean;
  end;

implementation

end.
