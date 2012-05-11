{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_StringConfigDataElementWithLanguage;

interface

uses
  i_JclNotify,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_StringConfigDataElement,
  i_StringByLanguage,
  i_LanguageManager,
  u_ConfigDataElementBase;

type
  TStringConfigDataElementWithLanguage = class(TConfigDataElementBase, IStringConfigDataElement)
  private
    FLanguageManager: ILanguageManager;
    FDefValuesByLanguage: IStringByLanguage;
    FUseSotre: Boolean;
    FStoreIdentifier: string;
    FIsStoreDefault: Boolean;

    FValue: string;
    FLangIndex: Integer;
    FLangChangeListener: IJclListener;
    procedure OnLangChange;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetValue: string;
    procedure SetValue(const AValue: string);

    function GetDefaultValue: string;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ADefValuesByLanguage: IStringByLanguage;
      AUseSotre: Boolean;
      const AStoreIdentifier: string;
      AIsStoreDefault: Boolean
    );
    destructor Destroy; override;
  end;


implementation

uses
  u_NotifyEventListener;

{ TStringConfigDataElementWithLanguage }

constructor TStringConfigDataElementWithLanguage.Create(
  const ALanguageManager: ILanguageManager;
  const ADefValuesByLanguage: IStringByLanguage;
  AUseSotre: Boolean;
  const AStoreIdentifier: string;
  AIsStoreDefault: Boolean
);
begin
  inherited Create;
  FLanguageManager := ALanguageManager;
  FDefValuesByLanguage := ADefValuesByLanguage;
  FUseSotre := AUseSotre;
  FStoreIdentifier := AStoreIdentifier;
  FIsStoreDefault := AIsStoreDefault;

  FLangChangeListener := TNotifyNoMmgEventListener.Create(Self.OnLangChange);
  FLanguageManager.GetChangeNotifier.Add(FLangChangeListener);

  FLangIndex := FLanguageManager.CurrentLanguageIndex;
  FValue := FDefValuesByLanguage.GetString(FLangIndex);
end;

destructor TStringConfigDataElementWithLanguage.Destroy;
begin
  FLanguageManager.GetChangeNotifier.Remove(FLangChangeListener);
  FLangChangeListener := nil;
  FLanguageManager := nil;
  inherited;
end;

procedure TStringConfigDataElementWithLanguage.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  if FUseSotre then begin
    if AConfigData <> nil then begin
      SetValue(AConfigData.ReadString(FStoreIdentifier, FValue));
    end;
  end;
end;

procedure TStringConfigDataElementWithLanguage.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
var
  VDefValue: string;
begin
  inherited;
  if FUseSotre then begin
    VDefValue := FDefValuesByLanguage.GetString(FLangIndex);
    if (FValue <> VDefValue) or FIsStoreDefault then begin
      AConfigData.WriteString(FStoreIdentifier, FValue);
    end else begin
      AConfigData.DeleteValue(FStoreIdentifier);
    end;
  end;
end;

function TStringConfigDataElementWithLanguage.GetDefaultValue: string;
begin
  LockRead;
  try
    Result := FDefValuesByLanguage.GetString(FLangIndex);
  finally
    UnlockRead;
  end;
end;

function TStringConfigDataElementWithLanguage.GetValue: string;
begin
  LockRead;
  try
    Result := FValue;
  finally
    UnlockRead;
  end;
end;

procedure TStringConfigDataElementWithLanguage.OnLangChange;
var
  VNewIndex: Integer;
  VDefValue: string;
begin
  LockWrite;
  try
    VNewIndex := FLanguageManager.CurrentLanguageIndex;
    VDefValue := FDefValuesByLanguage.GetString(FLangIndex);
    if FValue = VDefValue then begin
      SetValue(FDefValuesByLanguage.GetString(VNewIndex));
    end;
    FLangIndex := VNewIndex;
  finally
    UnlockWrite;
  end;
end;

procedure TStringConfigDataElementWithLanguage.SetValue(const AValue: string);
begin
  LockWrite;
  try
    if FValue <> AValue then begin
      FValue := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
