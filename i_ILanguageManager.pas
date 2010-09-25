unit i_ILanguageManager;

interface

uses
  Classes,
  i_JclNotify;

type
  ILanguageManager = interface
  ['{F8D76CED-2681-4DD4-AB24-4C6ECE89CE4D}']
    function GetCurrentLanguageCode: string;
    procedure SetCurrentLanguage(ACode: string);
    procedure GetInstalledLanguageCodes(list: TStrings);
    function GetLanguageNameByCode(ACode: string): WideString;
    function GetLangSelectNotifier: IJclNotifier;
    procedure GetLangNames(AList: TStrings);
    function GetCount: Integer;
    function GetCurrentLangIndex: Integer;
    procedure SetCurrentLangIndex(AValue: Integer);
    function GetIndexByLangCode(ACode: string): Integer;
    function GetLangCodeByIndex(AIndex: Integer): string;
    function GetLangNameByIndex(AIndex: Integer): string;
  end;

implementation

end.
