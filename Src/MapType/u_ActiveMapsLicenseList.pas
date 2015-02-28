unit u_ActiveMapsLicenseList;

interface

uses
  i_Listener,
  i_StringListStatic,
  i_StringListChangeable,
  i_MapTypeSetChangeable,
  i_LanguageManager,
  u_ChangeableBase;

type
  TActiveMapsLicenseList = class(TChangeableWithSimpleLockBase, IStringListChangeable)
  private
    FMapsSet: IMapTypeSetChangeable;
    FLanguageManager: ILanguageManager;

    FMapsListListener: IListener;
    FLanguageManagerListener: IListener;
    FStatic: IStringListStatic;
    procedure OnMapsListChange;
    procedure OnLangChange;
    function CreateStatic: IStringListStatic;
  private
    function GetStatic: IStringListStatic;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMapsSet: IMapTypeSetChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  i_MapType,
  i_MapTypeSet,
  u_ListenerByEvent,
  u_StringListStatic;

{ TActiveMapsLicenseList }

constructor TActiveMapsLicenseList.Create(
  const ALanguageManager: ILanguageManager;
  const AMapsSet: IMapTypeSetChangeable
);
begin
  Assert(ALanguageManager <> nil);
  Assert(AMapsSet <> nil);
  inherited Create;
  FMapsSet := AMapsSet;
  FLanguageManager := ALanguageManager;

  FMapsListListener := TNotifyNoMmgEventListener.Create(Self.OnMapsListChange);
  FMapsSet.ChangeNotifier.Add(FMapsListListener);

  FLanguageManagerListener := TNotifyNoMmgEventListener.Create(Self.OnLangChange);
  FLanguageManager.ChangeNotifier.Add(FLanguageManagerListener);
  FStatic := CreateStatic;
end;

destructor TActiveMapsLicenseList.Destroy;
begin
  if Assigned(FMapsSet) and Assigned(FMapsListListener) then begin
    FMapsSet.ChangeNotifier.Remove(FMapsListListener);
    FMapsSet := nil;
    FMapsListListener := nil;
  end;
  if Assigned(FLanguageManager) and Assigned(FLanguageManagerListener) then begin
    FLanguageManager.ChangeNotifier.Remove(FLanguageManagerListener);
    FLanguageManager := nil;
    FLanguageManagerListener := nil;
  end;
  inherited;
end;

function TActiveMapsLicenseList.CreateStatic: IStringListStatic;
var
  VStatic: IStringListStatic;
  VStringList: TStringList;
  VMapsSet: IMapTypeSet;
  VMapType: IMapType;
  VLangIndex: Integer;
  VLicense: string;
  i: Integer;
begin
  Result := nil;
  VMapsSet := FMapsSet.GetStatic;
  if Assigned(VMapsSet) then begin
    VStringList := TStringList.Create;
    try
      VLangIndex := FLanguageManager.CurrentLanguageIndex;
      for i := 0 to VMapsSet.Count - 1 do begin
        VMapType := VMapsSet.Items[i];
        Assert(Assigned(VMapType));
        VLicense := VMapType.Zmp.License.GetString(VLangIndex);
        if VLicense <> '' then begin
          VStringList.Add(VLicense);
        end;
      end;
      VStatic := TStringListStatic.CreateWithOwn(VStringList);
      VStringList := nil;
    finally
      VStringList.Free;
    end;
    Result := VStatic;
  end;
end;

function TActiveMapsLicenseList.GetStatic: IStringListStatic;
begin
  CS.BeginRead;
  try
    Result := FStatic;
  finally
    CS.EndRead;
  end;
end;

procedure TActiveMapsLicenseList.OnLangChange;
begin
  CS.BeginWrite;
  try
    FStatic := CreateStatic;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

procedure TActiveMapsLicenseList.OnMapsListChange;
begin
  CS.BeginWrite;
  try
    FStatic := CreateStatic;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

end.
