unit u_LanguageManager;

interface

uses
  Windows,
  Classes,
  IniFiles,
  i_JclNotify,
  i_ILanguageManager,
  u_LanguagesEx;

type
  TLanguageManager = class(TInterfacedObject, ILanguageManager)
  private
    FNotifier: IJclNotifier;
    FIniFile: TCustomIniFile;
    FDefaultLangCode: string;
    FLanguagesEx: TLanguagesEx;
    FNames : TStringList;
    FCodes : TStringList;
    procedure Add(AName, ACode : string);
    procedure LoadLangs;
  protected
    function GetCurrentLanguageCode: string;
    procedure SetCurrentLanguage(ACode: string);
    procedure GetInstalledLanguageCodes(list: TStrings);
    function GetLanguageNameByCode(ACode: string): WideString;
    function GetLangSelectNotifier: IJclNotifier;
  public
    constructor Create(AIni: TCustomIniFile);
    destructor Destroy; override;
  end;



implementation

uses
  gnugettext,
  u_JclNotify;

{ TLanguageManager }

procedure TLanguageManager.Add(AName, ACode: string);
begin
  FNames.Add(AName);
  FCodes.Add(ACode);
end;

constructor TLanguageManager.Create(AIni: TCustomIniFile);
begin
  FIniFile := AIni;
  FNotifier := TJclBaseNotifier.Create;
  FCodes := TStringList.Create;
  FNames := TStringList.Create;
  FDefaultLangCode := 'ru';

  LoadLangs;
end;

destructor TLanguageManager.Destroy;
begin
  FNames.Free;
  FCodes.Free;
  inherited;
end;

function TLanguageManager.GetCurrentLanguageCode: string;
begin
  Result := GetCurrentLanguage;  
end;

procedure TLanguageManager.GetInstalledLanguageCodes(list: TStrings);
begin
  list.Clear;
  list.Assign(FCodes);
end;

function TLanguageManager.GetLangSelectNotifier: IJclNotifier;
begin
  Result := FNotifier;
end;

function TLanguageManager.GetLanguageNameByCode(ACode: string): WideString;
var codeIndex : integer;
begin
  // by default, result is empty
  Result := '';
  // try to find code in the list
  codeIndex := FCodes.IndexOf(ACode);
  if codeIndex > -1 then begin
    // if directly found return the associated name
    Result := FNames[codeIndex];
  end else begin
    // else, if length is longer than 2 (hence, includes country info)
    if length(ACode) > 2 then begin
      // then try to find the language code alone
      codeIndex := FCodes.IndexOf(Copy(ACode, 1, 2));
      if codeIndex > -1 then begin
        // if found, return it
        Result := FNames[codeIndex];
      end;
    end;
  end;
end;

procedure TLanguageManager.LoadLangs;
var
  VinstalledLanguages: TStringList;
  i: Integer;
  VDefaultFound: Boolean;
  id : LCID;
begin
  // at first, default is not found
  VDefaultFound := false;
  // then get the installed languages
  VinstalledLanguages := TStringList.Create;
  try
    // get languages as a list of codes
    DefaultInstance.GetListOfLanguages('default', VinstalledLanguages);
    // add them into the list
    for i := 0 to VinstalledLanguages.Count - 1 do begin
      VDefaultFound := VDefaultFound or (VinstalledLanguages[i] = FDefaultLangCode);
      id := FLanguagesEx.GNUGetTextID[VinstalledLanguages[i]];
      add(dgettext('languages', FLanguagesEx.EngNameFromLocaleID[id]), VinstalledLanguages[i]);
    end;

    // and always add default, if not already there, this is the default
    if not VDefaultFound then begin
      id := FLanguagesEx.GNUGetTextID[FDefaultLangCode];
      add(dgettext('languages', FLanguagesEx.EngNameFromLocaleID[id]), FDefaultLangCode);
    end;
  finally
    VinstalledLanguages.Free;
  end;
end;

procedure TLanguageManager.SetCurrentLanguage(ACode: string);
begin
  UseLanguage(ACode);
end;

end.
