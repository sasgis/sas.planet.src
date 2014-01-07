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

unit u_GeoCoderListBase;

interface

uses
  SysUtils,
  ActiveX,
  i_GeoCoderList,
  i_GUIDSet,
  u_BaseInterfacedObject;

type
  TGeoCoderListBase = class(TBaseInterfacedObject, IGeoCoderList)
  private
    FList: IGUIDInterfaceSet;
    FCS: IReadWriteSync;
  protected
    procedure Add(const AItem: IGeoCoderListEntity);
  private
    function GetGUIDEnum: IEnumGUID;
    function Get(const AGUID: TGUID): IGeoCoderListEntity;
  public
    constructor Create;
  end;

implementation

uses
  u_Synchronizer,
  u_GUIDInterfaceSet;

{ TGeoCoderListBase }

constructor TGeoCoderListBase.Create;
begin
  inherited Create;
  FCS := MakeSyncRW_Std(Self, TRUE);
  FList := TGUIDInterfaceSet.Create(False);
end;

procedure TGeoCoderListBase.Add(const AItem: IGeoCoderListEntity);
begin
  FCS.BeginWrite;
  try
    FList.Add(AItem.GetGUID, AItem);
  finally
    FCS.EndWrite;
  end;
end;

function TGeoCoderListBase.Get(const AGUID: TGUID): IGeoCoderListEntity;
begin
  FCS.BeginRead;
  try
    Result := IGeoCoderListEntity(FList.GetByGUID(AGUID));
  finally
    FCS.EndRead;
  end;
end;

function TGeoCoderListBase.GetGUIDEnum: IEnumGUID;
begin
  FCS.BeginRead;
  try
    Result := FList.GetGUIDEnum;
  finally
    FCS.EndRead;
  end;
end;

end.
