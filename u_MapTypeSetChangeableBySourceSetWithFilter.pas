unit u_MapTypeSetChangeableBySourceSetWithFilter;

interface

uses
  i_Listener,
  i_MapTypes,
  u_ConfigDataElementBase;

type
  TMapTypeSetChangeableBySourceSetWithFilter = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IMapTypeSetChangeable)
  private
    FMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
    FSourceSet: IMapTypeSetChangeable;
    FSourceSetListener: IListener;

    FPrevSourceSetStatic: IMapTypeSet;
    procedure OnActiveMapsSetChange;
  private
    function GetStatic: IMapTypeSet;
  protected
    function CreateStatic: IInterface; override;
  protected
    function IsValidMapType(const AMapType: IMapType): Boolean; virtual;
  public
    constructor Create(
      const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
      const ASourceSet: IMapTypeSetChangeable
    );
    destructor Destroy; override;
  end;

type
  TMapTypeSetChangeableBySourceSetWithFilterBitmap = class(TMapTypeSetChangeableBySourceSetWithFilter)
  protected
    function IsValidMapType(const AMapType: IMapType): Boolean; override;
  end;

type
  TMapTypeSetChangeableBySourceSetWithFilterVector = class(TMapTypeSetChangeableBySourceSetWithFilter)
  protected
    function IsValidMapType(const AMapType: IMapType): Boolean; override;
  end;

  type
  TMapTypeSetChangeableBySourceSetWithFilterLicenseNotEmpty = class(TMapTypeSetChangeableBySourceSetWithFilter)
  protected
    function IsValidMapType(const AMapType: IMapType): Boolean; override;
  end;

implementation

uses
  ActiveX,
  u_ListenerByEvent;

{ TMapTypeSetChangeableBySourceSetWithFilter }

constructor TMapTypeSetChangeableBySourceSetWithFilter.Create(
  const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
  const ASourceSet: IMapTypeSetChangeable
);
begin
  inherited Create;
  FMapTypeSetBuilderFactory := AMapTypeSetBuilderFactory;
  FSourceSet := ASourceSet;

  FSourceSetListener := TNotifyNoMmgEventListener.Create(Self.OnActiveMapsSetChange);
  FSourceSet.ChangeNotifier.Add(FSourceSetListener);

  FPrevSourceSetStatic := FSourceSet.GetStatic;
end;

destructor TMapTypeSetChangeableBySourceSetWithFilter.Destroy;
begin
  if Assigned(FSourceSet) and Assigned(FSourceSetListener) then begin
    FSourceSet.ChangeNotifier.Remove(FSourceSetListener);
    FSourceSetListener := nil;
    FSourceSet := nil;
  end;
  inherited;
end;

function TMapTypeSetChangeableBySourceSetWithFilter.CreateStatic: IInterface;
var
  VResult: IMapTypeSetBuilder;
  VEnum: IEnumGUID;
  VGuid: TGUID;
  VCnt: Cardinal;
  VMapType: IMapType;
begin
  VResult := FMapTypeSetBuilderFactory.Build(False);
  if FPrevSourceSetStatic <> nil then begin
    VEnum := FPrevSourceSetStatic.GetIterator;
    while VEnum.Next(1, VGuid, VCnt) = S_OK do begin
      VMapType := FPrevSourceSetStatic.GetMapTypeByGUID(VGuid);
      if VMapType <> nil then begin
        if IsValidMapType(VMapType) then begin
          VResult.Add(VMapType);
        end;
      end;
    end;
  end;
  Result := VResult.MakeAndClear;
end;

function TMapTypeSetChangeableBySourceSetWithFilter.GetStatic: IMapTypeSet;
begin
  Result := IMapTypeSet(GetStaticInternal);
end;

function TMapTypeSetChangeableBySourceSetWithFilter.IsValidMapType(
  const AMapType: IMapType): Boolean;
begin
  Result := True;
end;

procedure TMapTypeSetChangeableBySourceSetWithFilter.OnActiveMapsSetChange;
var
  VNewSet: IMapTypeSet;
  VEnum: IEnumGUID;
  VGuid: TGUID;
  VCnt: Cardinal;
  VMapType: IMapType;
  VChanged: Boolean;
begin
  VNewSet := FSourceSet.GetStatic;
  LockWrite;
  try
    if (FPrevSourceSetStatic <> nil) and FPrevSourceSetStatic.IsEqual(VNewSet) then begin
      Exit;
    end;
    VChanged := False;
    if FPrevSourceSetStatic <> nil then begin
      VEnum := FPrevSourceSetStatic.GetIterator;
      while VEnum.Next(1, VGuid, VCnt) = S_OK do begin
        if (VNewSet = nil) or (VNewSet.GetMapTypeByGUID(VGuid) = nil) then begin
          VMapType := FPrevSourceSetStatic.GetMapTypeByGUID(VGuid);
          if VMapType <> nil then begin
            if IsValidMapType(VMapType) then begin
              VChanged := True;
            end;
          end;
        end;
      end;
    end;
    if VNewSet <> nil then begin
      VEnum := VNewSet.GetIterator;
      while VEnum.Next(1, VGuid, VCnt) = S_OK do begin
        if (FPrevSourceSetStatic = nil) or (FPrevSourceSetStatic.GetMapTypeByGUID(VGuid) = nil) then begin
          VMapType := VNewSet.GetMapTypeByGUID(VGuid);
          if VMapType <> nil then begin
            if IsValidMapType(VMapType) then begin
              VChanged := True;
            end;
          end;
        end;
      end;
    end;
    FPrevSourceSetStatic := VNewSet;
    if VChanged then begin
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

{ TMapTypeSetChangeableBySourceSetWithFilterBitmap }

function TMapTypeSetChangeableBySourceSetWithFilterBitmap.IsValidMapType(
  const AMapType: IMapType
): Boolean;
begin
  Result := (AMapType <> nil) and (AMapType.MapType.IsBitmapTiles);
end;

{ TMapTypeSetChangeableBySourceSetWithFilterVector }

function TMapTypeSetChangeableBySourceSetWithFilterVector.IsValidMapType(
  const AMapType: IMapType
): Boolean;
begin
  Result := (AMapType <> nil) and (AMapType.MapType.IsKmlTiles);
end;

{ TMapTypeSetChangeableBySourceSetWithFilterLicenseNotEmpty }

function TMapTypeSetChangeableBySourceSetWithFilterLicenseNotEmpty.IsValidMapType(
  const AMapType: IMapType): Boolean;
begin
  Result := False;
  if AMapType <> nil then begin
    Result := AMapType.MapType.Zmp.License.GetDefault <> '';
  end;
end;

end.
