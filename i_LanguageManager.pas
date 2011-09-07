unit i_LanguageManager;

interface

uses
  Classes,
  i_ConfigDataElement,
  i_LanguageListStatic;

type
  ILanguageManager = interface(IConfigDataElement)
  ['{F8D76CED-2681-4DD4-AB24-4C6ECE89CE4D}']
    function GetCurrentLanguageCode: string;
    procedure SetCurrentLanguageCode(ACode: string);

    function GetCurrentLangIndex: Integer;
    procedure SetCurrentLangIndex(AValue: Integer);

    function GetLanguageList: ILanguageListStatic;
    property LanguageList: ILanguageListStatic read GetLanguageList;

    procedure GetInstalledLanguageCodes(list: TStrings);
    function GetLanguageNameByCode(ACode: string): WideString;
    procedure GetLangNames(AList: TStrings);
    function GetCount: Integer;
    function GetIndexByLangCode(ACode: string): Integer;
    function GetLangCodeByIndex(AIndex: Integer): string;
    function GetLangNameByIndex(AIndex: Integer): string;
  end;

implementation

end.
