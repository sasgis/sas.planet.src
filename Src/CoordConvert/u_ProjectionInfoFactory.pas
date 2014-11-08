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

unit u_ProjectionInfoFactory;

interface

uses
  SysUtils,
  i_IDList,
  i_HashFunction,
  i_CoordConverter,
  i_ProjectionInfo,
  i_CoordConverterFactory,
  u_BaseInterfacedObject;

type
  TProjectionInfoFactory = class(TBaseInterfacedObject, IProjectionInfoFactory)
  private
    FHashFunction: IHashFunction;
    FSync: IReadWriteSync;
    FProjectionsByConverter: IIDInterfaceList;
  private
    function GetByConverterAndZoom(
      const AGeoConverter: ICoordConverter;
      AZoom: Byte
    ): IProjectionInfo;
  public
    constructor Create(
      const AHashFunction: IHashFunction;
      const ASync: IReadWriteSync
    );
  end;

implementation

uses
  t_Hash,
  u_IDInterfaceList,
  u_ProjectionInfo;

{ TProjectionInfoFactory }

constructor TProjectionInfoFactory.Create(
  const AHashFunction: IHashFunction;
  const ASync: IReadWriteSync
);
begin
  inherited Create;
  FHashFunction := AHashFunction;
  FSync := ASync;

  FProjectionsByConverter := TIDInterfaceList.Create(False);
end;

function TProjectionInfoFactory.GetByConverterAndZoom(
  const AGeoConverter: ICoordConverter;
  AZoom: Byte
): IProjectionInfo;
var
  VID: Integer;
  VZooms: IIDInterfaceList;
  i: Integer;
  VProjection: IProjectionInfo;
  VHash: THashValue;
begin
  Assert(Assigned(AGeoConverter));
  Result := nil;
  if not Assigned(AGeoConverter) then begin
    Exit;
  end;

  VID := Integer(AGeoConverter);
  FSync.BeginRead;
  try
    if not Supports(FProjectionsByConverter.GetByID(VID), IIDInterfaceList, VZooms) then begin
      VZooms := nil;
    end;
  finally
    FSync.EndRead;
  end;
  if VZooms <> nil then begin
    Result := IProjectionInfo(VZooms.GetByID(AZoom));
  end else begin
    VZooms := TIDInterfaceList.Create(False);
    for i := AGeoConverter.MinZoom to AGeoConverter.MaxZoom do begin
      VHash := AGeoConverter.Hash;
      FHashFunction.UpdateHashByInteger(VHash, i);
      VProjection := TProjectionInfo.Create(VHash, AGeoConverter, i);
      VZooms.Add(i, VProjection);
      if i = AZoom then begin
        Result := VProjection;
      end;
    end;
    FSync.BeginWrite;
    try
      FProjectionsByConverter.Replace(VID, VZooms);
    finally
      FSync.EndWrite;
    end;
  end;
end;

end.
