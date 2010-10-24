unit u_SaveLoadTBConfigByConfigProvider;

interface

uses
  Classes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider;

procedure TBConfigProviderLoadPositions(const OwnerComponent: TComponent;
  const AConfigProvider: IConfigDataProvider);

procedure TBConfigProviderSavePositions(const OwnerComponent: TComponent;
  const AConfigProvider: IConfigDataWriteProvider);

implementation

uses
  TB2Dock;

function ConfigProviderReadInt(const ToolbarName, Value: String; const Default: Longint;
  const ExtraData: TTBPositionExtraData): Longint;
var
  VConfigProvider: IConfigDataProvider;
begin
  if ExtraData <> nil then begin
    VConfigProvider := IConfigDataProvider(ExtraData).GetSubItem(ToolbarName);
    if VConfigProvider <> nil then begin
      Result := VConfigProvider.ReadInteger(Value, Default);
    end else begin
      Result := Default;
    end;
  end else begin
    Result := Default;
  end;
end;

function ConfigProviderReadString(const ToolbarName, Value, Default: String;
  const ExtraData: TTBPositionExtraData): String;
var
  VConfigProvider: IConfigDataProvider;
begin
  if ExtraData <> nil then begin
    VConfigProvider := IConfigDataProvider(ExtraData).GetSubItem(ToolbarName);
    if VConfigProvider <> nil then begin
      Result := VConfigProvider.ReadString(Value, Default);
    end else begin
      Result := Default;
    end;
  end else begin
    Result := Default;
  end;
end;

procedure ConfigProviderWriteInt(const ToolbarName, Value: String; const Data: Longint;
  const ExtraData: TTBPositionExtraData);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  VConfigProvider := IConfigDataWriteProvider(ExtraData).GetOrCreateSubItem(ToolbarName);
  VConfigProvider.WriteInteger(Value, Data);
end;

procedure ConfigProviderWriteString(const ToolbarName, Value, Data: String;
  const ExtraData: TTBPositionExtraData);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  VConfigProvider := IConfigDataWriteProvider(ExtraData).GetOrCreateSubItem(ToolbarName);
  VConfigProvider.WriteString(Value, Data);
end;

procedure TBConfigProviderLoadPositions(const OwnerComponent: TComponent;
  const AConfigProvider: IConfigDataProvider);
begin
  if AConfigProvider <> nil then begin
    TBCustomLoadPositions(OwnerComponent, ConfigProviderReadInt, ConfigProviderReadString, Pointer(AConfigProvider));
  end;
end;

procedure TBConfigProviderSavePositions(const OwnerComponent: TComponent;
  const AConfigProvider: IConfigDataWriteProvider);
begin
  TBCustomSavePositions(OwnerComponent, ConfigProviderWriteInt, ConfigProviderWriteString, Pointer(AConfigProvider));
end;

end.
