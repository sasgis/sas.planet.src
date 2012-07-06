{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_PathDetalizeProviderListBase;

interface

uses
  ActiveX,
  u_ConfigDataElementBase,
  i_GUIDSet,
  i_PathDetalizeProviderList;

type
  TPathDetalizeProviderListBase = class(TConfigDataElementBaseEmptySaveLoad, IPathDetalizeProviderList)
  private
    FList: IGUIDInterfaceSet;
  private
    function GetGUIDEnum: IEnumGUID;
    function Get(const AGUID: TGUID): IPathDetalizeProviderListEntity;
  protected
    procedure Add(const AItem: IPathDetalizeProviderListEntity);
  public
    constructor Create;
  end;

implementation

uses
  u_GUIDInterfaceSet;

{ TPathDetalizeProviderListBase }

constructor TPathDetalizeProviderListBase.Create;
begin
  inherited Create;
  FList := TGUIDInterfaceSet.Create(False);
end;

procedure TPathDetalizeProviderListBase.Add(
  const AItem: IPathDetalizeProviderListEntity
);
begin
  LockWrite;
  try
    if not FList.IsExists(AItem.GUID) then begin
      FList.Add(AItem.GUID, AItem);
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TPathDetalizeProviderListBase.Get(
  const AGUID: TGUID): IPathDetalizeProviderListEntity;
begin
  LockRead;
  try
    Result := IPathDetalizeProviderListEntity(FList.GetByGUID(AGUID));
  finally
    UnlockRead;
  end;
end;

function TPathDetalizeProviderListBase.GetGUIDEnum: IEnumGUID;
begin
  Result := FList.GetGUIDEnum;
end;

end.
