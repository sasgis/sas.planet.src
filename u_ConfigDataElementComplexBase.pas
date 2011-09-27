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

unit u_ConfigDataElementComplexBase;

interface

uses
  Classes,
  Contnrs,
  i_JclNotify,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ConfigDataElement,
  i_ConfigSaveLoadStrategy,
  u_ConfigDataElementBase;

type
  TConfigDataElementComplexBase = class(TConfigDataElementBase)
  private
    FList: TObjectList;
    FItemChangeListener: IJclListener;
    procedure OnItemChange(Sender: TObject);
  protected
    procedure DoSubItemChange; virtual;
    procedure Add(AItem: IConfigDataElement; ASaveLoadStrategy: IConfigSaveLoadStrategy); overload;
    procedure Add(
      AItem: IConfigDataElement;
      ASaveLoadStrategy: IConfigSaveLoadStrategy;
      ANeedReadLock: Boolean;
      ANeedWriteLock: Boolean;
      ANeedStopNotify: Boolean;
      ANeedChangedListen: Boolean
    ); overload;
    function GetItemsCount: Integer;
    function GetItem(AIndex: Integer): IConfigDataElement;
    function GetSaveLoadStrategy(AIndex: Integer): IConfigSaveLoadStrategy;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    procedure StopNotify; override;
    procedure StartNotify; override;
    procedure LockWrite; override;
    procedure UnlockWrite; override;
    procedure LockRead; override;
    procedure UnlockRead; override;
  public
    constructor Create();
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_NotifyEventListener;

type
  TSubItemInfo = class
  private
    FItem: IConfigDataElement;
    FSaveLoadStrategy: IConfigSaveLoadStrategy;
    FNeedReadLock: Boolean;
    FNeedWriteLock: Boolean;
    FNeedStopNotify: Boolean;
    FNeedChangedListen: Boolean;
  public
    constructor Create(
      AItem: IConfigDataElement;
      ASaveLoadStrategy: IConfigSaveLoadStrategy;
      ANeedReadLock: Boolean;
      ANeedWriteLock: Boolean;
      ANeedStopNotify: Boolean;
      ANeedChangedListen: Boolean
    );
    destructor Destroy; override;

    property Item: IConfigDataElement read FItem;
    property SaveLoadStrategy: IConfigSaveLoadStrategy read FSaveLoadStrategy;
    property NeedReadLock: Boolean read FNeedReadLock;
    property NeedWriteLock: Boolean read FNeedWriteLock;
    property NeedStopNotify: Boolean read FNeedStopNotify;
    property NeedChangedListen: Boolean read FNeedChangedListen;
  end;

{ TSubItemInfo }

constructor TSubItemInfo.Create(AItem: IConfigDataElement;
  ASaveLoadStrategy: IConfigSaveLoadStrategy; ANeedReadLock, ANeedWriteLock,
  ANeedStopNotify, ANeedChangedListen: Boolean);
begin
  FItem := AItem;
  FSaveLoadStrategy := ASaveLoadStrategy;
  FNeedReadLock := ANeedReadLock;
  FNeedWriteLock := ANeedWriteLock;
  FNeedStopNotify := ANeedStopNotify;
  FNeedChangedListen := ANeedChangedListen;
end;

destructor TSubItemInfo.Destroy;
begin
  FItem := nil;
  FSaveLoadStrategy := nil;
  inherited;
end;

{ TConfigDataElementComplexBase }

constructor TConfigDataElementComplexBase.Create;
begin
  inherited;
  FList := TObjectList.Create(True);
  FItemChangeListener := TNotifyEventListener.Create(Self.OnItemChange);
end;

destructor TConfigDataElementComplexBase.Destroy;
var
  i: Integer;
  VItem: TSubItemInfo;
begin
  for i := 0 to GetItemsCount - 1 do begin
    VItem := TSubItemInfo(FList[i]);
    if VItem.NeedChangedListen then begin
      VItem.Item.GetChangeNotifier.Remove(FItemChangeListener);
    end;
  end;
  FItemChangeListener := nil;
  FreeAndNil(FList);
  inherited;
end;

procedure TConfigDataElementComplexBase.Add(AItem: IConfigDataElement;
  ASaveLoadStrategy: IConfigSaveLoadStrategy; ANeedReadLock, ANeedWriteLock,
  ANeedStopNotify, ANeedChangedListen: Boolean);
var
  VItem: TSubItemInfo;
begin
  VItem := TSubItemInfo.Create(
    AItem,
    ASaveLoadStrategy,
    ANeedReadLock,
    ANeedWriteLock,
    ANeedStopNotify,
    ANeedChangedListen
  );
  FList.Add(VItem);
  if VItem.NeedChangedListen then begin
    VItem.Item.GetChangeNotifier.Add(FItemChangeListener);
  end;
end;

procedure TConfigDataElementComplexBase.Add(
  AItem: IConfigDataElement;
  ASaveLoadStrategy: IConfigSaveLoadStrategy
);
begin
  Add(AItem, ASaveLoadStrategy, True, True, True, True);
end;

procedure TConfigDataElementComplexBase.DoReadConfig(
  AConfigData: IConfigDataProvider);
var
  i: Integer;
  VItem: TSubItemInfo;
begin
  inherited;
  for i := 0 to GetItemsCount - 1 do begin
    VItem := TSubItemInfo(FList[i]);
    if VItem.SaveLoadStrategy <> nil then begin
      VItem.SaveLoadStrategy.ReadConfig(AConfigData, VItem.Item);
    end;
  end;
end;

procedure TConfigDataElementComplexBase.DoSubItemChange;
begin
end;

procedure TConfigDataElementComplexBase.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
var
  i: Integer;
  VItem: TSubItemInfo;
begin
  inherited;
  for i := 0 to GetItemsCount - 1 do begin
    VItem := TSubItemInfo(FList[i]);
    if VItem.SaveLoadStrategy <> nil then begin
      VItem.SaveLoadStrategy.WriteConfig(AConfigData, VItem.Item);
    end;
  end;
end;

function TConfigDataElementComplexBase.GetSaveLoadStrategy(
  AIndex: Integer): IConfigSaveLoadStrategy;
var
  VItem: TSubItemInfo;
begin
  VItem := TSubItemInfo(FList[AIndex]);
  Result := VItem.SaveLoadStrategy;
end;

procedure TConfigDataElementComplexBase.LockRead;
var
  i: Integer;
  VItem: TSubItemInfo;
begin
  inherited;
  for i := 0 to GetItemsCount - 1 do begin
    VItem := TSubItemInfo(FList[i]);
    if VItem.NeedReadLock then begin
      VItem.Item.LockRead;
    end;
  end;
end;

procedure TConfigDataElementComplexBase.LockWrite;
var
  i: Integer;
  VItem: TSubItemInfo;
begin
  inherited;
  for i := 0 to GetItemsCount - 1 do begin
    VItem := TSubItemInfo(FList[i]);
    if VItem.NeedWriteLock then begin
      VItem.Item.LockWrite;
    end;
  end;
end;

function TConfigDataElementComplexBase.GetItem(
  AIndex: Integer): IConfigDataElement;
var
  VItem: TSubItemInfo;
begin
  VItem := TSubItemInfo(FList[AIndex]);
  Result := VItem.Item;
end;

function TConfigDataElementComplexBase.GetItemsCount: Integer;
begin
  Result := FList.Count;
end;

procedure TConfigDataElementComplexBase.OnItemChange(Sender: TObject);
begin
  inherited StopNotify;
  try
    DoSubItemChange;
    SetChanged;
  finally
    inherited StartNotify;
  end;
end;

procedure TConfigDataElementComplexBase.StartNotify;
var
  i: Integer;
  VItem: TSubItemInfo;
begin
  for i := 0 to GetItemsCount - 1 do begin
    VItem := TSubItemInfo(FList[i]);
    if VItem.FNeedStopNotify then begin
      VItem.Item.StartNotify;
    end;
  end;
  inherited;
end;

procedure TConfigDataElementComplexBase.StopNotify;
var
  i: Integer;
  VItem: TSubItemInfo;
begin
  inherited;
  for i := 0 to GetItemsCount - 1 do begin
    VItem := TSubItemInfo(FList[i]);
    if VItem.FNeedStopNotify then begin
      VItem.Item.StopNotify;
    end;
  end;
end;

procedure TConfigDataElementComplexBase.UnlockRead;
var
  i: Integer;
  VItem: TSubItemInfo;
begin
  for i := 0 to GetItemsCount - 1 do begin
    VItem := TSubItemInfo(FList[i]);
    if VItem.NeedReadLock then begin
      VItem.Item.UnlockRead;
    end;
  end;
  inherited;
end;

procedure TConfigDataElementComplexBase.UnlockWrite;
var
  i: Integer;
  VItem: TSubItemInfo;
begin
  for i := 0 to GetItemsCount - 1 do begin
    VItem := TSubItemInfo(FList[i]);
    if VItem.NeedWriteLock then begin
      VItem.Item.UnlockWrite;
    end;
  end;
  inherited;
end;

end.
