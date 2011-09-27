{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
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
    procedure OnLangChange(Sender: TObject);
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetValue: string;
    procedure SetValue(AValue: string);

    function GetDefaultValue: string;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      ADefValuesByLanguage: IStringByLanguage;
      AUseSotre: Boolean;
      AStoreIdentifier: string;
      AIsStoreDefault: Boolean
    );
    destructor Destroy; override;
  end;


implementation

uses
  u_NotifyEventListener;

{ TStringConfigDataElementWithLanguage }

constructor TStringConfigDataElementWithLanguage.Create(
  ALanguageManager: ILanguageManager;
  ADefValuesByLanguage: IStringByLanguage;
  AUseSotre: Boolean;
  AStoreIdentifier: string;
  AIsStoreDefault: Boolean
);
begin
  inherited Create;
  FLanguageManager := ALanguageManager;
  FDefValuesByLanguage := ADefValuesByLanguage;
  FUseSotre := AUseSotre;
  FStoreIdentifier := AStoreIdentifier;
  FIsStoreDefault := AIsStoreDefault;

  FLangChangeListener := TNotifyEventListener.Create(Self.OnLangChange);
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
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if FUseSotre then begin
    if AConfigData <> nil then begin
      SetValue(AConfigData.ReadString(FStoreIdentifier, FValue));
    end;
  end;
end;

procedure TStringConfigDataElementWithLanguage.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
var
  VDefValue: string;
begin
  inherited;
  if FUseSotre then begin
    VDefValue := FDefValuesByLanguage.GetString(FLangIndex);
    if (FValue <> VDefValue) or FIsStoreDefault   then begin
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

procedure TStringConfigDataElementWithLanguage.OnLangChange(Sender: TObject);
var
  VNewIndex: Integer;
  VDefValue: string;
begin
  LockWrite;
  try
    VNewIndex := FLanguageManager.CurrentLanguageIndex;
    VDefValue := FDefValuesByLanguage.GetString(FLangIndex);
    if FValue = VDefValue  then begin
      SetValue(FDefValuesByLanguage.GetString(VNewIndex));
    end;
    FLangIndex := VNewIndex;
  finally
    UnlockWrite;
  end;
end;

procedure TStringConfigDataElementWithLanguage.SetValue(AValue: string);
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
