{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_StringConfigDataElementWithDefBase;

interface

uses
  i_Notifier,
  i_Listener,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_StringConfigDataElement,
  i_LanguageManager,
  u_ConfigDataElementBase;

type
  TStringConfigDataElementWithDefBase = class(TConfigDataElementBase, IStringConfigDataElement)
  private
    FLanguageManager: ILanguageManager;
    FUseSotre: Boolean;
    FStoreIdentifier: string;
    FIsStoreDefault: Boolean;

    FDefValue: string;
    FValue: string;
    FLangChangeListener: IListener;
    procedure OnLangChange;
  protected
    property LanguageManager: ILanguageManager read FLanguageManager;
    function GetDefValueForCurrentLang: string; virtual; abstract;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetValue: string;
    procedure SetValue(const AValue: string);

    function GetDefaultValue: string;
  public
    procedure AfterConstruction; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      AUseSotre: Boolean;
      const AStoreIdentifier: string;
      AIsStoreDefault: Boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent;

{ TStringConfigDataElementWithDefByStringRec }

constructor TStringConfigDataElementWithDefBase.Create(
  const ALanguageManager: ILanguageManager;
  AUseSotre: Boolean;
  const AStoreIdentifier: string;
  AIsStoreDefault: Boolean
);
begin
  inherited Create;
  FLanguageManager := ALanguageManager;
  FUseSotre := AUseSotre;
  FStoreIdentifier := AStoreIdentifier;
  FIsStoreDefault := AIsStoreDefault;

  FLangChangeListener := TNotifyNoMmgEventListener.Create(Self.OnLangChange);
  FLanguageManager.GetChangeNotifier.Add(FLangChangeListener);
end;

destructor TStringConfigDataElementWithDefBase.Destroy;
begin
  if Assigned(FLanguageManager) and Assigned(FLangChangeListener) then begin
    FLanguageManager.GetChangeNotifier.Remove(FLangChangeListener);
    FLangChangeListener := nil;
    FLanguageManager := nil;
  end;
  inherited;
end;

procedure TStringConfigDataElementWithDefBase.AfterConstruction;
begin
  inherited;
  FDefValue := GetDefValueForCurrentLang;
  FValue := FDefValue;
end;

procedure TStringConfigDataElementWithDefBase.DoReadConfig(
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

procedure TStringConfigDataElementWithDefBase.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  if FUseSotre then begin
    if (FValue <> FDefValue) or FIsStoreDefault then begin
      AConfigData.WriteString(FStoreIdentifier, FValue);
    end else begin
      AConfigData.DeleteValue(FStoreIdentifier);
    end;
  end;
end;

function TStringConfigDataElementWithDefBase.GetDefaultValue: string;
begin
  LockRead;
  try
    Result := FDefValue;
  finally
    UnlockRead;
  end;
end;

function TStringConfigDataElementWithDefBase.GetValue: string;
begin
  LockRead;
  try
    Result := FValue;
  finally
    UnlockRead;
  end;
end;

procedure TStringConfigDataElementWithDefBase.OnLangChange;
var
  VDefValueNew: string;
begin
  LockWrite;
  try
    VDefValueNew := GetDefValueForCurrentLang;
    if FValue = FDefValue then begin
      SetValue(VDefValueNew);
    end;
    FDefValue := VDefValueNew;
  finally
    UnlockWrite;
  end;
end;

procedure TStringConfigDataElementWithDefBase.SetValue(const AValue: string);
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
