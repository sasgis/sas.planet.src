unit u_ConfigDataElementComplexBase;

interface

uses
  Classes,
  i_JclNotify,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IConfigDataElement,
  i_IConfigSaveLoadStrategy,
  u_ConfigDataElementBase;

type
  TConfigDataElementComplexBase = class(TConfigDataElementBase)
  private
    FList: IInterfaceList;
    FStrategyList: IInterfaceList;
    FItemChangeListener: IJclListener;
    procedure OnItemChange(Sender: TObject);
  protected
    procedure DoSubItemChange; virtual;
    procedure Add(AItem: IConfigDataElement; ASaveLoadStrategy: IConfigSaveLoadStrategy);
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

{ TConfigDataElementComplexBase }

constructor TConfigDataElementComplexBase.Create;
begin
  inherited;
  FList := TInterfaceList.Create;
  FStrategyList := TInterfaceList.Create;
  FItemChangeListener := TNotifyEventListener.Create(Self.OnItemChange);
end;

destructor TConfigDataElementComplexBase.Destroy;
var
  i: Integer;
begin
  for i := 0 to GetItemsCount - 1 do begin
    GetItem(i).GetChangeNotifier.Remove(FItemChangeListener);
  end;
  FItemChangeListener := nil;
  FList := nil;
  FStrategyList := nil;
  inherited;
end;

procedure TConfigDataElementComplexBase.Add(
  AItem: IConfigDataElement;
  ASaveLoadStrategy: IConfigSaveLoadStrategy
);
begin
  FStrategyList.Add(ASaveLoadStrategy);
  FList.Add(AItem);
  AItem.GetChangeNotifier.Add(FItemChangeListener);
end;

procedure TConfigDataElementComplexBase.DoReadConfig(
  AConfigData: IConfigDataProvider);
var
  i: Integer;
  VStrategy: IConfigSaveLoadStrategy;
begin
  inherited;
  for i := 0 to GetItemsCount - 1 do begin
    VStrategy := GetSaveLoadStrategy(i);
    if VStrategy <> nil then begin
      VStrategy.ReadConfig(AConfigData, GetItem(i));
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
  VStrategy: IConfigSaveLoadStrategy;
begin
  inherited;
  for i := 0 to GetItemsCount - 1 do begin
    VStrategy := GetSaveLoadStrategy(i);
    if VStrategy <> nil then begin
      VStrategy.WriteConfig(AConfigData, GetItem(i));
    end;
  end;
end;

function TConfigDataElementComplexBase.GetSaveLoadStrategy(
  AIndex: Integer): IConfigSaveLoadStrategy;
begin
  Result := IConfigSaveLoadStrategy(FStrategyList.Items[AIndex]);
end;

procedure TConfigDataElementComplexBase.LockRead;
var
  i: Integer;
begin
  inherited;
  for i := 0 to GetItemsCount - 1 do begin
    GetItem(i).LockRead;
  end;
end;

procedure TConfigDataElementComplexBase.LockWrite;
var
  i: Integer;
begin
  inherited;
  for i := 0 to GetItemsCount - 1 do begin
    GetItem(i).LockWrite;
  end;
end;

function TConfigDataElementComplexBase.GetItem(
  AIndex: Integer): IConfigDataElement;
begin
  Result := IConfigDataElement(FList.Items[AIndex]);
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
begin
  for i := 0 to GetItemsCount - 1 do begin
    GetItem(i).StartNotify;
  end;
  inherited;
end;

procedure TConfigDataElementComplexBase.StopNotify;
var
  i: Integer;
begin
  inherited;
  for i := 0 to GetItemsCount - 1 do begin
    GetItem(i).StopNotify;
  end;
end;

procedure TConfigDataElementComplexBase.UnlockRead;
var
  i: Integer;
begin
  for i := 0 to GetItemsCount - 1 do begin
    GetItem(i).UnlockRead;
  end;
  inherited;
end;

procedure TConfigDataElementComplexBase.UnlockWrite;
var
  i: Integer;
begin
  for i := 0 to GetItemsCount - 1 do begin
    GetItem(i).UnlockWrite;
  end;
  inherited;
end;

end.
