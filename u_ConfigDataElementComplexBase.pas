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
    FItemChangeListener: IJclListener;
  protected
    procedure OnItemChange(Sender: TObject);
    procedure Add(AItem: IConfigDataElement);
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
  u_NotifyEventListener;

{ TConfigDataElementComplexBase }

procedure TConfigDataElementComplexBase.Add(AItem: IConfigDataElement);
begin
  FList.Add(AItem);
  AItem.GetChangeNotifier.Add(FItemChangeListener);
end;

constructor TConfigDataElementComplexBase.Create;
begin
  FList := TInterfaceList.Create;
  FItemChangeListener := TNotifyEventListener.Create(Self.OnItemChange);
end;

destructor TConfigDataElementComplexBase.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do begin
    IConfigDataElement(FList.Items[i]).GetChangeNotifier.Remove(FItemChangeListener);
  end;
  FItemChangeListener := nil;
  FList := nil;
  inherited;
end;

procedure TConfigDataElementComplexBase.DoReadConfig(
  AConfigData: IConfigDataProvider);
var
  i: Integer;
begin
  inherited;
  for i := 0 to FList.Count - 1 do begin
    IConfigDataElement(FList.Items[i]).ReadConfig(AConfigData);
  end;
end;

procedure TConfigDataElementComplexBase.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
var
  i: Integer;
begin
  inherited;
  for i := 0 to FList.Count - 1 do begin
    IConfigDataElement(FList.Items[i]).WriteConfig(AConfigData);
  end;
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
  for i := 0 to FList.Count - 1 do begin
    IConfigDataElement(FList.Items[i]).StartNotify;
  end;
  inherited;
end;

procedure TConfigDataElementComplexBase.StopNotify;
var
  i: Integer;
begin
  inherited;
  for i := 0 to FList.Count - 1 do begin
    IConfigDataElement(FList.Items[i]).StopNotify;
  end;
end;

end.
