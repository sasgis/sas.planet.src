{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  SyncObjs,
  ActiveX,
  i_JclNotify,
  i_GeoCoderList,
  i_GUIDSet;

type
  TGeoCoderListBase = class(TInterfacedObject, IGeoCoderList)
  private
    FList: IGUIDInterfaceSet;
    FCS: TCriticalSection;
    FAddNotifier: IJclNotifier;
  protected
    procedure Add(AItem: IGeoCoderListEntity);
  protected
    function GetGUIDEnum: IEnumGUID;
    function Get(AGUID: TGUID): IGeoCoderListEntity;
    function GetAddNotifier: IJclNotifier;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_JclNotify,
  u_GUIDInterfaceSet;

{ TGeoCoderListBase }

constructor TGeoCoderListBase.Create;
begin
  FCS := TCriticalSection.Create;
  FList := TGUIDInterfaceSet.Create(False);
  FAddNotifier := TJclBaseNotifier.Create;
end;

destructor TGeoCoderListBase.Destroy;
begin
  FreeAndNil(FCS);
  FList := nil;
  inherited;
end;

procedure TGeoCoderListBase.Add(AItem: IGeoCoderListEntity);
begin
  FCS.Acquire;
  try
    FList.Add(AItem.GetGUID, AItem);
  finally
    FCS.Release;
  end;
  FAddNotifier.Notify(nil);
end;

function TGeoCoderListBase.Get(AGUID: TGUID): IGeoCoderListEntity;
begin
  FCS.Acquire;
  try
    Result := IGeoCoderListEntity(FList.GetByGUID(AGUID));
  finally
    FCS.Release;
  end;
end;

function TGeoCoderListBase.GetAddNotifier: IJclNotifier;
begin
  Result := FAddNotifier;
end;

function TGeoCoderListBase.GetGUIDEnum: IEnumGUID;
begin
  FCS.Acquire;
  try
    Result := FList.GetGUIDEnum;
  finally
    FCS.Release;
  end;
end;

end.
