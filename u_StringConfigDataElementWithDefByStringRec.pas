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
      AResStringRec: PResStringRec;
      AUseSotre: Boolean;
      AStoreIdentifier: string;
      AIsStoreDefault: Boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_NotifyEventListener;

{ TStringConfigDataElementWithDefByStringRec }

constructor TStringConfigDataElementWithDefByStringRec.Create(
  ALanguageManager: ILanguageManager; AResStringRec: PResStringRec;
  AUseSotre: Boolean; AStoreIdentifier: string; AIsStoreDefault: Boolean);
begin
  inherited Create;
  FLanguageManager := ALanguageManager;
  FUseSotre := AUseSotre;
  FStoreIdentifier := AStoreIdentifier;
  FIsStoreDefault := AIsStoreDefault;
  FResStringRec := AResStringRec;

  FLangChangeListener := TNotifyEventListener.Create(Self.OnLangChange);
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
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if FUseSotre then begin
    if AConfigData <> nil then begin
      SetValue(AConfigData.ReadString(FStoreIdentifier, FValue));
    end;
  end;
end;

procedure TStringConfigDataElementWithDefByStringRec.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  if FUseSotre then begin
    if (FValue <> FDefValue) or FIsStoreDefault   then begin
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

procedure TStringConfigDataElementWithDefByStringRec.OnLangChange(
  Sender: TObject);
var
  VDefValueNew: string;
begin
  LockWrite;
  try
    VDefValueNew := LoadResString(FResStringRec);
    if FValue = FDefValue  then begin
      SetValue(VDefValueNew);
    end;
    FDefValue := VDefValueNew;
  finally
    UnlockWrite;
  end;
end;

procedure TStringConfigDataElementWithDefByStringRec.SetValue(AValue: string);
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
