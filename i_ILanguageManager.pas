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
  end;

implementation

end.
