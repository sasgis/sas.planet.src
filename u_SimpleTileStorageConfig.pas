unit u_SimpleTileStorageConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_SimpleTileStorageConfig,
  u_ConfigDataElementBase;

type
  TSimpleTileStorageConfig = class(TConfigDataElementBase, ISimpleTileStorageConfig)
  private
    FDefConfig: ISimpleTileStorageConfigStatic;

    FCacheTypeCode: Integer;
    FNameInCache: string;
    FIsReadOnly: boolean;
    FAllowDelete: boolean;
    FAllowAdd: boolean;
    FAllowReplace: boolean;

    FStatic: ISimpleTileStorageConfigStatic;
    function CreateStatic: ISimpleTileStorageConfigStatic;
  protected
    procedure DoBeforeChangeNotify; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetCacheTypeCode: Integer;
    procedure SetCacheTypeCode(AValue: Integer);

    function GetNameInCache: string;
    procedure SetNameInCache(AValue: string);

    function GetTileFileExt: string;
    function GetIsStoreFileCache: Boolean;

    function GetIsReadOnly: boolean;
    procedure SetIsReadOnly(AValue: Boolean);

    function GetAllowDelete: boolean;
    procedure SetAllowDelete(AValue: Boolean);

    function GetAllowAdd: boolean;
    procedure SetAllowAdd(AValue: Boolean);

    function GetAllowReplace: boolean;
    procedure SetAllowReplace(AValue: Boolean);

    function GetStatic: ISimpleTileStorageConfigStatic;
  public
    constructor Create(ADefConfig: ISimpleTileStorageConfigStatic);
  end;

implementation

uses
  u_SimpleTileStorageConfigStatic;

{ TSimpleTileStorageConfig }

constructor TSimpleTileStorageConfig.Create(
  ADefConfig: ISimpleTileStorageConfigStatic);
begin
  inherited Create;
  FDefConfig := ADefConfig;

  FCacheTypeCode := FDefConfig.CacheTypeCode;
  FNameInCache := FDefConfig.NameInCache;

  FIsReadOnly := FDefConfig.IsReadOnly;
  FAllowDelete := FDefConfig.AllowDelete;
  FAllowAdd := FDefConfig.AllowAdd;
  FAllowReplace := FDefConfig.AllowReplace;
end;

function TSimpleTileStorageConfig.CreateStatic: ISimpleTileStorageConfigStatic;
begin
  Result :=
    TSimpleTileStorageConfigStatic.Create(
      FCacheTypeCode,
      FNameInCache,
      FDefConfig.TileFileExt,
      FDefConfig.IsStoreFileCache,
      FIsReadOnly,
      FAllowDelete,
      FAllowAdd,
      FAllowReplace
    );
end;

procedure TSimpleTileStorageConfig.DoBeforeChangeNotify;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
  end;
end;

procedure TSimpleTileStorageConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetCacheTypeCode(AConfigData.ReadInteger('CacheType', FCacheTypeCode));
    SetNameInCache(AConfigData.ReadString('NameInCache', FNameInCache));
    SetIsReadOnly(AConfigData.ReadBool('IsReadOnly', FIsReadOnly));
    SetChanged;
  end;
end;

procedure TSimpleTileStorageConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  if FCacheTypeCode <> FDefConfig.CacheTypeCode then begin
    AConfigData.WriteInteger('CacheType', FCacheTypeCode);
  end else begin
    AConfigData.DeleteValue('CacheType');
  end;
  if FNameInCache <> FDefConfig.NameInCache then begin
    AConfigData.WriteString('NameInCache', FNameInCache);
  end else begin
    AConfigData.DeleteValue('NameInCache');
  end;
  if FIsReadOnly <> FDefConfig.IsReadOnly then begin
    AConfigData.WriteBool('IsReadOnly', FIsReadOnly);
  end else begin
    AConfigData.DeleteValue('IsReadOnly');
  end;
end;

function TSimpleTileStorageConfig.GetAllowAdd: boolean;
begin
  LockRead;
  try
    Result := FAllowAdd;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetAllowDelete: boolean;
begin
  LockRead;
  try
    Result := FAllowDelete;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetAllowReplace: boolean;
begin
  LockRead;
  try
    Result := FAllowReplace;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetCacheTypeCode: Integer;
begin
  LockRead;
  try
    Result := FCacheTypeCode;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetIsReadOnly: boolean;
begin
  LockRead;
  try
    Result := FIsReadOnly;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetIsStoreFileCache: Boolean;
begin
  Result := FDefConfig.IsStoreFileCache;
end;

function TSimpleTileStorageConfig.GetNameInCache: string;
begin
  LockRead;
  try
    Result := FNameInCache;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetStatic: ISimpleTileStorageConfigStatic;
begin
  Result := FStatic;
end;

function TSimpleTileStorageConfig.GetTileFileExt: string;
begin
  Result := FDefConfig.TileFileExt;
end;

procedure TSimpleTileStorageConfig.SetAllowAdd(AValue: Boolean);
var
  VValue: Boolean;
begin
  LockWrite;
  try
    VValue := FDefConfig.AllowAdd and (not FIsReadOnly) and AValue;
    if FAllowAdd <> VValue then begin
      FAllowAdd := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSimpleTileStorageConfig.SetAllowDelete(AValue: Boolean);
var
  VValue: Boolean;
begin
  LockWrite;
  try
    VValue := FDefConfig.AllowDelete and (not FIsReadOnly) and AValue;
    if FAllowDelete <> VValue then begin
      FAllowDelete := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSimpleTileStorageConfig.SetAllowReplace(AValue: Boolean);
var
  VValue: Boolean;
begin
  LockWrite;
  try
    VValue := FDefConfig.AllowReplace and (not FIsReadOnly) and AValue;
    if FAllowReplace <> VValue then begin
      FAllowReplace := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSimpleTileStorageConfig.SetCacheTypeCode(AValue: Integer);
begin
  if FDefConfig.CacheTypeCode <> 5  then begin
    if AValue <> 5 then begin
      LockWrite;
      try
        if FCacheTypeCode <> AValue then begin
          FCacheTypeCode := AValue;
          SetChanged;
        end;
      finally
        UnlockWrite;
      end;
    end;
  end;
end;

procedure TSimpleTileStorageConfig.SetIsReadOnly(AValue: Boolean);
var
  VValue: Boolean;
begin
  LockWrite;
  try
    VValue := FDefConfig.IsReadOnly or (FNameInCache = '') or AValue;
    if FIsReadOnly <> VValue then begin
      FIsReadOnly := VValue;
      SetAllowDelete(FAllowDelete);
      SetAllowAdd(FAllowAdd);
      SetAllowReplace(FAllowReplace);
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSimpleTileStorageConfig.SetNameInCache(AValue: string);
begin
  LockWrite;
  try
    if FNameInCache <> AValue then begin
      FNameInCache := AValue;
      SetIsReadOnly(FIsReadOnly);
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
