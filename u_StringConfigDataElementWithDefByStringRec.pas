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

unit u_StringConfigDataElementWithDefByStringRec;

interface

uses
  i_JclNotify,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_StringConfigDataElement,
  i_LanguageManager,
  u_ConfigDataElementBase;

type
  TStringConfigDataElementWithDefByStringRec = class(TConfigDataElementBase, IStringConfigDataElement)
  private
    FLanguageManager: ILanguageManager;
    FUseSotre: Boolean;
    FStoreIdentifier: string;
    FIsStoreDefault: Boolean;

    FResStringRec: PResStringRec;
    FDefValue: string;
    FValue: string;
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
      AResStringRec: PResStringRec;
      AUseSotre: Boolean;
      const AStoreIdentifier: string;
      AIsStoreDefault: Boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_NotifyEventListener;

{ TStringConfigDataElementWithDefByStringRec }

constructor TStringConfigDataElementWithDefByStringRec.Create(
  const ALanguageManager: ILanguageManager;
  AResStringRec: PResStringRec;
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
  FResStringRec := AResStringRec;

  FLangChangeListener := TNotifyNoMmgEventListener.Create(Self.OnLangChange);
  FLanguageManager.GetChangeNotifier.Add(FLangChangeListener);
  FDefValue := LoadResString(FResStringRec);
  FValue := FDefValue;
end;

destructor TStringConfigDataElementWithDefByStringRec.Destroy;
begin
  FLanguageManager.GetChangeNotifier.Remove(FLangChangeListener);
  FLangChangeListener := nil;
  FLanguageManager := nil;
  inherited;
end;

procedure TStringConfigDataElementWithDefByStringRec.DoReadConfig(
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

procedure TStringConfigDataElementWithDefByStringRec.DoWriteConfig(
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

function TStringConfigDataElementWithDefByStringRec.GetDefaultValue: string;
begin
  LockRead;
  try
    Result := FDefValue;
  finally
    UnlockRead;
  end;
end;

function TStringConfigDataElementWithDefByStringRec.GetValue: string;
begin
  LockRead;
  try
    Result := FValue;
  finally
    UnlockRead;
  end;
end;

procedure TStringConfigDataElementWithDefByStringRec.OnLangChange;
var
  VDefValueNew: string;
begin
  LockWrite;
  try
    VDefValueNew := LoadResString(FResStringRec);
    if FValue = FDefValue then begin
      SetValue(VDefValueNew);
    end;
    FDefValue := VDefValueNew;
  finally
    UnlockWrite;
  end;
end;

procedure TStringConfigDataElementWithDefByStringRec.SetValue(const AValue: string);
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
