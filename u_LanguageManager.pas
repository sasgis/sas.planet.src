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
    function GetIndexByCode(ACode: string): Integer;
    procedure LoadLangs;
    procedure SetTranslateIgnore;
    procedure AfterLangChange;
  protected
    function GetCurrentLanguageCode: string;
    procedure SetCurrentLanguage(ACode: string);
    procedure GetInstalledLanguageCodes(AList: TStrings);
    function GetLanguageNameByCode(ACode: string): WideString;
    function GetLangSelectNotifier: IJclNotifier;
    procedure GetLangNames(AList: TStrings);
    function GetCount: Integer;
    function GetCurrentLangIndex: Integer;
    procedure SetCurrentLangIndex(AValue: Integer);
    function GetIndexByLangCode(ACode: string): Integer;
    function GetLangCodeByIndex(AIndex: Integer): string;
    function GetLangNameByIndex(AIndex: Integer): string;
  public
    constructor Create(AIni: TCustomIniFile);
    destructor Destroy; override;
  end;



implementation

uses
  SysUtils,
  Forms,
  Dialogs,
  Controls,
  ActnList,
  Graphics,
  GR32,
  EmbeddedWB,
  gnugettext,
  u_JclNotify,
  u_CommonFormAndFrameParents;

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
  FLanguagesEx := TLanguagesEx.Create;

  SetTranslateIgnore;

  LoadLangs;
  if FIniFile <> nil then begin
    SetCurrentLanguage(FIniFile.ReadString('VIEW', 'Lang', ''));
  end;
end;

destructor TLanguageManager.Destroy;
begin
  if FIniFile <> nil then begin
    FIniFile.WriteString('VIEW', 'Lang', GetCurrentLanguageCode);
  end;
  FNotifier := nil;
  FreeAndNil(FNames);
  FreeAndNil(FCodes);
  FreeAndNil(FLanguagesEx);
  inherited;
end;

function TLanguageManager.GetCount: Integer;
begin
  Result := FCodes.Count;
end;

function TLanguageManager.GetCurrentLangIndex: Integer;
begin
  Result := GetIndexByLangCode(GetCurrentLanguage);
end;

function TLanguageManager.GetCurrentLanguageCode: string;
var
  VIndex: Integer;
begin
  VIndex := GetCurrentLangIndex;
  if VIndex >= 0 then begin
    Result := FCodes[VIndex];
  end else begin
    Result := '';
  end;
end;

function TLanguageManager.GetIndexByCode(ACode: string): Integer;
begin
  // try to find code in the list
  Result := FCodes.IndexOf(ACode);
  if Result < 0 then begin
    // else, if length is longer than 2 (hence, includes country info)
    if length(ACode) > 2 then begin
      // then try to find the language code alone
      Result := FCodes.IndexOf(Copy(ACode, 1, 2));
    end;
  end;
end;

function TLanguageManager.GetIndexByLangCode(ACode: string): Integer;
begin
  Result := GetIndexByCode(ACode);
  if Result < 0 then begin
    Result := GetIndexByCode(FDefaultLangCode);
  end;
end;

procedure TLanguageManager.GetInstalledLanguageCodes(AList: TStrings);
begin
  AList.Clear;
  AList.Assign(FCodes);
end;

function TLanguageManager.GetLangCodeByIndex(AIndex: Integer): string;
begin
  Result := FCodes[AIndex];
end;

function TLanguageManager.GetLangNameByIndex(AIndex: Integer): string;
begin
  Result := FNames[AIndex];
end;

procedure TLanguageManager.GetLangNames(AList: TStrings);
begin
  AList.Clear;
  AList.Assign(FNames);
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
  codeIndex := GetIndexByCode(ACode);
  if codeIndex >= 0 then begin
    Result := FNames[codeIndex];
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
    for i := 0 to VinstalledLanguages.Count - 1 do begin
      VDefaultFound := VDefaultFound or (VinstalledLanguages[i] = FDefaultLangCode);
    end;

    // and always add default, if not already there, this is the default
    if not VDefaultFound then begin
      id := FLanguagesEx.GNUGetTextID[FDefaultLangCode];
      add(FLanguagesEx.EngNameFromLocaleID[id], FDefaultLangCode);
    end;

    // add them into the list
    for i := 0 to VinstalledLanguages.Count - 1 do begin
      VDefaultFound := VDefaultFound or (VinstalledLanguages[i] = FDefaultLangCode);
      id := FLanguagesEx.GNUGetTextID[VinstalledLanguages[i]];
      add(FLanguagesEx.EngNameFromLocaleID[id], VinstalledLanguages[i]);
    end;
  finally
    VinstalledLanguages.Free;
  end;
end;

procedure TLanguageManager.AfterLangChange;
var
  i: Integer;
begin
  // force reloading forms with new selection
  for i := 0 to application.ComponentCount - 1 do begin
    if application.Components[i] is TCommonFormParent then begin
      TCommonFormParent(application.Components[i]).RefreshTranslation;
    end else if application.Components[i] is TCommonFrameParent then begin
      TCommonFrameParent(application.Components[i]).RefreshTranslation;
    end;
  end;
  FNotifier.Notify(nil);
end;

procedure TLanguageManager.SetCurrentLangIndex(AValue: Integer);
var
  VLastUsedCode: string;
begin
  VLastUsedCode := GetCurrentLanguage;
  if AValue >= 0 then begin
    UseLanguage(FCodes[AValue]);
  end else begin
    UseLanguage(FDefaultLangCode);
  end;
  if VLastUsedCode <> GetCurrentLanguage then begin
    AfterLangChange;
  end;
end;

procedure TLanguageManager.SetCurrentLanguage(ACode: string);
var
  VIndex: Integer;
begin
  VIndex := GetIndexByCode(ACode);
  SetCurrentLangIndex(VIndex);
end;

procedure TLanguageManager.SetTranslateIgnore;
begin
  TP_GlobalIgnoreClass(TFont);
  TP_GlobalIgnoreClassProperty(TAction,'Category');
  TP_GlobalIgnoreClassProperty(TControl,'HelpKeyword');
  TP_GlobalIgnoreClassProperty(TWinControl,'ImeName');
  TP_GlobalIgnoreClassProperty(TEmbeddedWB,'StatusText');
  TP_GlobalIgnoreClassProperty(TEmbeddedWB,'UserAgent');
  TP_GlobalIgnoreClassProperty(TEmbeddedWB,'About');
  TP_GlobalIgnoreClassProperty(TOpenDialog,'DefaultExt');
  TP_GlobalIgnoreClassProperty(TCustomBitmap32,'ResamplerClassName');

end;

end.
