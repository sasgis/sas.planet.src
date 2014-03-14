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

unit u_ConfigSaveLoadStrategyBasicProviderSubItem;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ConfigDataElement,
  i_ConfigSaveLoadStrategy,
  u_BaseInterfacedObject;

type
  TConfigSaveLoadStrategyBasicProviderSubItem = class(TBaseInterfacedObject, IConfigSaveLoadStrategy)
  private
    FSubItemName: string;
  private
    procedure WriteConfig(
      const AProvider: IConfigDataWriteProvider;
      const AElement: IConfigDataElement
    );
    procedure ReadConfig(
      const AProvider: IConfigDataProvider;
      const AElement: IConfigDataElement
    );
  public
    constructor Create(const ASubItemName: string);
  end;

implementation

{ TConfigSaveLoadStrategyBasicProviderSubItem }

constructor TConfigSaveLoadStrategyBasicProviderSubItem.Create(
  const ASubItemName: string
);
begin
  inherited Create;
  FSubItemName := ASubItemName;
end;

procedure TConfigSaveLoadStrategyBasicProviderSubItem.ReadConfig(
  const AProvider: IConfigDataProvider;
  const AElement: IConfigDataElement
);
var
  VProvider: IConfigDataProvider;
begin
  if AProvider = nil then begin
    VProvider := nil;
  end else begin
    VProvider := AProvider.GetSubItem(FSubItemName);
  end;
  AElement.ReadConfig(VProvider);
end;

procedure TConfigSaveLoadStrategyBasicProviderSubItem.WriteConfig(
  const AProvider: IConfigDataWriteProvider;
  const AElement: IConfigDataElement
);
var
  VProvider: IConfigDataWriteProvider;
begin
  if AProvider = nil then begin
    VProvider := nil;
  end else begin
    VProvider := AProvider.GetOrCreateSubItem(FSubItemName);
  end;
  AElement.WriteConfig(VProvider);
end;

end.
