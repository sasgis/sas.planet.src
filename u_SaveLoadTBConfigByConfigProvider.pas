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

unit u_SaveLoadTBConfigByConfigProvider;

interface

uses
  Classes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider;

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
