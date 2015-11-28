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

unit u_VectorTileProviderByInfoTileProvider;

interface

uses
  Types,
  SysUtils,
  i_NotifierOperation,
  i_VectorItemSubset,
  i_Projection,
  i_InfoTileProvider,
  i_VectorTileProvider,
  i_VectorDataFactory,
  u_BaseInterfacedObject;

type
  TVectorTileProviderByInfoTileProvider = class(TBaseInterfacedObject, IVectorTileProvider)
  private
    FProjection: IProjection;
    FSource: IInfoTileProvider;
    FVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
    FIsIgnoreError: Boolean;
  private
    function GetProjection: IProjection;
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IVectorItemSubset;
  public
    constructor Create(
      const AIsIgnoreError: Boolean;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const ASource: IInfoTileProvider
    );
  end;

type
  EBadContentTypeError = type Exception;

implementation

uses
  i_VectorDataLoader,
  i_ContentTypeInfo,
  i_BinaryData,
  i_TileInfoBasic;

{ TVectorTileProviderByInfoTileProvider }

constructor TVectorTileProviderByInfoTileProvider.Create(
  const AIsIgnoreError: Boolean;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const ASource: IInfoTileProvider
);
begin
  Assert(AVectorDataItemMainInfoFactory <> nil);
  Assert(ASource <> nil);
  inherited Create;
  FIsIgnoreError := AIsIgnoreError;
  FVectorDataItemMainInfoFactory := AVectorDataItemMainInfoFactory;
  FSource := ASource;
  FProjection := FSource.Projection;
end;

function TVectorTileProviderByInfoTileProvider.GetProjection: IProjection;
begin
  Result := FProjection;
end;

function TVectorTileProviderByInfoTileProvider.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IVectorItemSubset;
var
  VTileInfo: ITileInfoBasic;
  VTileInfoWithData: ITileInfoWithData;
  VBinary: IBinaryData;
  VContentType: IContentTypeInfoVectorData;
  VLoader: IVectorDataLoader;
begin
  Result := nil;
  try
    VTileInfo := FSource.GetTile(AOperationID, ACancelNotifier, ATile);
  except
    if not FIsIgnoreError then begin
      raise;
    end else begin
      Result := nil;
    end;
  end;
  if Supports(VTileInfo, ITileInfoWithData, VTileInfoWithData) then begin
    VBinary := VTileInfoWithData.TileData;
    if Assigned(VBinary) then begin
      if Supports(VTileInfoWithData.ContentType, IVectorDataLoader, VContentType) then begin
        VLoader := VContentType.GetLoader;
        if Assigned(VLoader) then begin
          try
            Result := VLoader.Load(VBinary, nil, FVectorDataItemMainInfoFactory);
          except
            if not FIsIgnoreError then begin
              raise;
            end else begin
              Result := nil;
            end;
          end;
        end else begin
          if not FIsIgnoreError then begin
            raise EBadContentTypeError.Create('No loader for this vector type');
          end;
        end;
      end else begin
        if not FIsIgnoreError then begin
          raise EBadContentTypeError.Create('Tile is not vector');
        end;
      end;
    end;
  end;
end;

end.
