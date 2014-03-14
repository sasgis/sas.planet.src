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

unit u_MarkerProviderForVectorItemFixedMarker;

interface

uses
  i_MarkerDrawable,
  i_VectorDataItemSimple,
  i_MarksDrawConfig,
  i_MarkerProviderForVectorItem,
  u_BaseInterfacedObject;

type
  TMarkerProviderForVectorItemFixedMarker = class(TBaseInterfacedObject, IMarkerProviderForVectorItem)
  private
    FMarker: IMarkerDrawableChangeable;
  private
    function GetMarker(
      const AConfig: ICaptionDrawConfigStatic;
      const AItem: IVectorDataItemSimple
    ): IMarkerDrawable;
  public
    constructor Create(const AMarker: IMarkerDrawableChangeable);
  end;

implementation

{ TMarkerProviderForVectorItemFixedMarker }

constructor TMarkerProviderForVectorItemFixedMarker.Create(
  const AMarker: IMarkerDrawableChangeable);
begin
  inherited Create;
  FMarker := AMarker;
end;

function TMarkerProviderForVectorItemFixedMarker.GetMarker(
  const AConfig: ICaptionDrawConfigStatic;
  const AItem: IVectorDataItemSimple
): IMarkerDrawable;
begin
  Result := FMarker.GetStatic;
end;

end.
