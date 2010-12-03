unit u_ConfigDataElementComplexBase;

interface

uses
  Classes,
  i_JclNotify,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IConfigDataElement,
  u_ConfigDataElementBase;

type
  TConfigDataElementComplexBase = class(TConfigDataElementBase)
  private
    FList: IInterfaceList;
    FNamesList: TStringList;
    FItemChangeListener: IJclListener;
  protected
    procedure OnItemChange(Sender: TObject);
    procedure Add(AItem: IConfigDataElement; AConfigSubItemName: string);
    function GetItemsCount: Integer;
    function GetItem(AIndex: Integer): IConfigDataElement;
    function GetConfigSubItemName(AIndex: Integer): string;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    procedure StopNotify; override;
    procedure StartNotify; override;
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
  FList := TInterfaceList.Create;
  FItemChangeListener := TNotifyEventListener.Create(Self.OnItemChange);
  FNamesList := TStringList.Create;
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
  FreeAndNil(FNamesList);
  inherited;
end;

procedure TConfigDataElementComplexBase.Add(
  AItem: IConfigDataElement;
  AConfigSubItemName: string
);
begin
  FNamesList.Add(AConfigSubItemName);
  FList.Add(AItem);
  AItem.GetChangeNotifier.Add(FItemChangeListener);
end;

procedure TConfigDataElementComplexBase.DoReadConfig(
  AConfigData: IConfigDataProvider);
var
  i: Integer;
  VConfigSubItemName: string;
  VConfigData: IConfigDataProvider;
begin
  inherited;
  for i := 0 to GetItemsCount - 1 do begin
    VConfigSubItemName := GetConfigSubItemName(i);
    if VConfigSubItemName = '' then begin
      VConfigData := AConfigData;
    end else begin
      if AConfigData = nil then begin
        VConfigData := nil;
      end else begin
        VConfigData := AConfigData.GetSubItem(VConfigSubItemName);
      end;
    end;
    GetItem(i).ReadConfig(VConfigData);
  end;
end;

procedure TConfigDataElementComplexBase.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
var
  i: Integer;
  VConfigSubItemName: string;
  VConfigData: IConfigDataWriteProvider;
begin
  inherited;
  for i := 0 to GetItemsCount - 1 do begin
    VConfigSubItemName := GetConfigSubItemName(i);
    if VConfigSubItemName = '' then begin
      VConfigData := AConfigData;
    end else begin
      VConfigData := AConfigData.GetOrCreateSubItem(VConfigSubItemName);
    end;
    GetItem(i).WriteConfig(VConfigData);
  end;
end;

function TConfigDataElementComplexBase.GetConfigSubItemName(
  AIndex: Integer): string;
begin
  Result := FNamesList.Strings[AIndex];
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

procedure TConfigDataElementComplexBase.OnItemChange;
begin
  inherited StopNotify;
  SetChanged;
  inherited StartNotify;
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

end.
