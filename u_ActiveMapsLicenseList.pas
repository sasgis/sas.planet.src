unit u_ActiveMapsLicenseList;

interface

uses
  i_Listener,
  i_StringListStatic,
  i_StringListChangeable,
  i_MapTypes,
  i_LanguageManager,
  u_ConfigDataElementBase;

type
  TActiveMapsLicenseList = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IStringListChangeable)
  private
    FMapsSet: IMapTypeSetChangeable;
    FLanguageManager: ILanguageManager;

    FMapsListListener: IListener;
    FLanguageManagerListener: IListener;
    procedure OnMapsListChange;
    procedure OnLangChange;
  protected
    function CreateStatic: IInterface; override;
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
  ActiveX,
  Classes,
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

function TActiveMapsLicenseList.CreateStatic: IInterface;
var
  VStatic: IStringListStatic;
  VStringList: TStringList;
  VMapsSet: IMapTypeSet;
  VMapType: IMapType;
  VLangIndex: Integer;
  VLicense: string;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  VCnt: Cardinal;
begin
  VStringList := TStringList.Create;
  try
    VLangIndex := FLanguageManager.CurrentLanguageIndex;
    VMapsSet := FMapsSet.GetStatic;
    VEnum := VMapsSet.GetIterator;
    while VEnum.Next(1, VGUID, VCnt) = S_OK do begin
      VMapType := VMapsSet.GetMapTypeByGUID(VGUID);
      if VMapType <> nil then begin
        VLicense := VMapType.MapType.Zmp.License.GetString(VLangIndex);
        if VLicense <> '' then begin
          VStringList.Add(VLicense);
        end;
      end;
    end;
    VStatic := TStringListStatic.CreateWithOwn(VStringList);
    VStringList := nil;
  finally
    VStringList.Free;
  end;
  Result := VStatic;
end;

function TActiveMapsLicenseList.GetStatic: IStringListStatic;
begin
  Result := IStringListStatic(GetStaticInternal);
end;

procedure TActiveMapsLicenseList.OnLangChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TActiveMapsLicenseList.OnMapsListChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
