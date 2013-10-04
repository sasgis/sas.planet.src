{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_LanguageManager;

interface

uses
  Windows,
  Classes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_LanguageListStatic,
  i_LanguageManager,
  u_ConfigDataElementBase;

type
  TLanguageManager = class(TConfigDataElementBase, ILanguageManager)
  private
    FList: ILanguageListStatic;
    FDefaultLangCode: string;
    FNames: TStringList;
    FLangRootPath: string;
    procedure LoadLangs;
    procedure SetTranslateIgnore;
  protected
    procedure DoBeforeChangeNotify; override;
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetCurrentLanguageCode: string;
    procedure SetCurrentLanguageCode(const ACode: string);

    function GetCurrentLanguageIndex: Integer;
    procedure SetCurrentLanguageIndex(const AValue: Integer);

    function GetLanguageList: ILanguageListStatic;
    function GetLangNameByIndex(const AIndex: Integer): string;
  public
    constructor Create(const ALangRootPath: string);
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
  i_FileNameIterator,
  u_FileNameIteratorInFolderByMask,
  u_LanguagesEx,
  u_CommonFormAndFrameParents,
  u_LanguageListStatic;

const
  cDefLangCode = 'en';
  cLangFileExt = '.mo';

{ TLanguageManager }

constructor TLanguageManager.Create(const ALangRootPath: string);
begin
  inherited Create;
  FLangRootPath := IncludeTrailingPathDelimiter(ALangRootPath);
  FNames := TStringList.Create;
  FDefaultLangCode := cDefLangCode;
  SetTranslateIgnore;
  LoadLangs;
end;

destructor TLanguageManager.Destroy;
begin
  FreeAndNil(FNames);
  inherited;
end;

procedure TLanguageManager.DoBeforeChangeNotify;
var
  i: Integer;
begin
  inherited;
  // force reloading forms with new selection
  for i := 0 to application.ComponentCount - 1 do begin
    if application.Components[i] is TCommonFormParent then begin
      TCommonFormParent(application.Components[i]).RefreshTranslation;
    end;
  end;
end;

procedure TLanguageManager.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetCurrentLanguageCode(AConfigData.ReadString('Lang', ''));
  end;
end;

procedure TLanguageManager.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteString('Lang', GetCurrentLanguageCode);
end;

function TLanguageManager.GetCurrentLanguageIndex: Integer;
begin
  if not FList.FindCode(GetCurrentLanguage, Result) then begin
    Result := 0;
  end;
end;

function TLanguageManager.GetCurrentLanguageCode: string;
var
  VIndex: Integer;
begin
  VIndex := GetCurrentLanguageIndex;
  if VIndex >= 0 then begin
    Result := FList.Code[VIndex];
  end else begin
    Result := '';
  end;
end;

function TLanguageManager.GetLangNameByIndex(const AIndex: Integer): string;
begin
  Result := FNames[AIndex];
end;

function TLanguageManager.GetLanguageList: ILanguageListStatic;
begin
  Result := FList;
end;

procedure TLanguageManager.LoadLangs;

  procedure Add(const ACodes: TStringList; const AName, ACode: string);
  begin
    FNames.Add(AName);
    ACodes.Add(ACode);
  end;

  procedure GetListOfLanguages(const AList: TStringList);
  var
    VIterator: IFileNameIterator;
    VFileNameW: WideString;
  begin
    VIterator := TFileNameIteratorInFolderByMask.Create(FLangRootPath, '', '*' + cLangFileExt, True);
    while VIterator.Next(VFileNameW) do begin
      AList.Add(StringReplace(ExtractFileName(VFileNameW), cLangFileExt, '', [rfReplaceAll, rfIgnoreCase]));
    end;
  end;

var
  VCodes: TStringList;
  VLanguagesEx: TLanguagesEx;
  VInstalledLanguages: TStringList;
  I: Integer;
  VLangCodeID: LCID;
  VLangFile: string;
  VCurrentCode: string;
  VCurrentIndex: Integer;
begin
  VCodes := TStringList.Create;
  try
    VLanguagesEx := TLanguagesEx.Create;
    try
      VLangCodeID := VLanguagesEx.GNUGetTextID[FDefaultLangCode];
      Add(VCodes, VLanguagesEx.EngNameFromLocaleID[VLangCodeID], FDefaultLangCode);

      VInstalledLanguages := TStringList.Create;
      try
        GetListOfLanguages(VInstalledLanguages);
        for I := 0 to VInstalledLanguages.Count - 1 do begin
          VLangCodeID := VLanguagesEx.GNUGetTextID[VInstalledLanguages[I]];
          if VLangCodeID <> 0 then begin
            Add(VCodes, VLanguagesEx.EngNameFromLocaleID[VLangCodeID], VInstalledLanguages[I]);
          end;
        end;
      finally
        VInstalledLanguages.Free;
      end;
      FList := TLanguageListStatic.Create(VCodes);

      VCurrentCode := DefaultInstance.GetCurrentLanguage;
      if not FList.FindCode(GetCurrentLanguage, VCurrentIndex) then begin
        VLangCodeID := VLanguagesEx.GNUGetTextID[VCurrentCode];
        VCurrentCode := VLanguagesEx.GNUGetTextName[VLangCodeID];
        DefaultInstance.UseLanguage(VCurrentCode);
        VLangFile := FLangRootPath + VCurrentCode + cLangFileExt;
        DefaultInstance.bindtextdomainToFile(DefaultTextDomain, VLangFile);
      end;
    finally
      VLanguagesEx.Free;
    end;
  finally
    VCodes.Free;
  end;
end;

procedure TLanguageManager.SetCurrentLanguageIndex(const AValue: Integer);
var
  VLastUsedCode: string;
  VCurrCode: string;
  VLangFile: string;
begin
  LockWrite;
  try
    VLastUsedCode := DefaultInstance.GetCurrentLanguage;
    if AValue >= 0 then begin
      DefaultInstance.UseLanguage(FList.Code[AValue]);
    end else begin
      DefaultInstance.UseLanguage(FDefaultLangCode);
    end;
    VCurrCode := GetCurrentLanguage;
    if VLastUsedCode <> VCurrCode then begin
      VLangFile := FLangRootPath + VCurrCode + cLangFileExt;
      DefaultInstance.bindtextdomainToFile(DefaultTextDomain, VLangFile);
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TLanguageManager.SetCurrentLanguageCode(const ACode: string);
var
  VIndex: Integer;
begin
  if FList.FindCode(ACode, VIndex) then begin
    SetCurrentLanguageIndex(VIndex);
  end;
end;

procedure TLanguageManager.SetTranslateIgnore;
begin
  TP_GlobalIgnoreClass(TFont);
  TP_GlobalIgnoreClassProperty(TAction, 'Category');
  TP_GlobalIgnoreClassProperty(TControl, 'HelpKeyword');
  TP_GlobalIgnoreClassProperty(TWinControl, 'ImeName');
  TP_GlobalIgnoreClassProperty(TEmbeddedWB, 'StatusText');
  TP_GlobalIgnoreClassProperty(TEmbeddedWB, 'UserAgent');
  TP_GlobalIgnoreClassProperty(TEmbeddedWB, 'About');
  TP_GlobalIgnoreClassProperty(TOpenDialog, 'DefaultExt');
  TP_GlobalIgnoreClassProperty(TCustomBitmap32, 'ResamplerClassName');  
end;

end.
