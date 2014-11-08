{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_SaveLoadTBConfigByConfigProvider;

interface

uses
  Classes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider;

procedure TBConfigProviderLoadPositions(
    const OwnerComponent: TComponent;
    const AConfigProvider: IConfigDataProvider
  );

procedure TBConfigProviderSavePositions(
    const OwnerComponent: TComponent;
    const AConfigProvider: IConfigDataWriteProvider
  );

implementation

uses
  TB2Dock;

function ConfigProviderReadInt(
  const ToolbarName, Value: String;
  const Default: Longint;
  const ExtraData: TTBPositionExtraData
): Longint;
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

function ConfigProviderReadString(
  const ToolbarName, Value, Default: String;
  const ExtraData: TTBPositionExtraData
): String;
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

procedure ConfigProviderWriteInt(
  const ToolbarName, Value: String;
  const Data: Longint;
  const ExtraData: TTBPositionExtraData
);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  VConfigProvider := IConfigDataWriteProvider(ExtraData).GetOrCreateSubItem(ToolbarName);
  VConfigProvider.WriteInteger(Value, Data);
end;

procedure ConfigProviderWriteString(
  const ToolbarName, Value, Data: String;
  const ExtraData: TTBPositionExtraData
);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  VConfigProvider := IConfigDataWriteProvider(ExtraData).GetOrCreateSubItem(ToolbarName);
  VConfigProvider.WriteString(Value, Data);
end;

procedure TBConfigProviderLoadPositions(
  const OwnerComponent: TComponent;
  const AConfigProvider: IConfigDataProvider
);
begin
  if AConfigProvider <> nil then begin
    TBCustomLoadPositions(OwnerComponent, ConfigProviderReadInt, ConfigProviderReadString, Pointer(AConfigProvider));
  end;
end;

procedure TBConfigProviderSavePositions(
  const OwnerComponent: TComponent;
  const AConfigProvider: IConfigDataWriteProvider
);
begin
  TBCustomSavePositions(OwnerComponent, ConfigProviderWriteInt, ConfigProviderWriteString, Pointer(AConfigProvider));
end;

end.
