{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_ProjectionSetSimple;

interface

uses
  t_Hash,
  i_ProjectionInfo,
  i_ProjectionSet,
  i_InterfaceListStatic,
  u_BaseInterfacedObject;

type
  TProjectionSetSimple = class(TBaseInterfacedObject, IProjectionSet)
  private
    FHash: THashValue;
    FZoomCount: Byte;
    FZooms: IInterfaceListStatic;
  private
    function GetHash: THashValue;
    function IsSame(const AProjectionSet: IProjectionSet): Boolean;

    function GetZoomCount: Byte;

    function GetZoom(const AIndex: Byte): IProjectionInfo;

    procedure ValidateZoom(var AZoom: Byte);
    function CheckZoom(const AZoom: Byte): Boolean;

    function GetSuitableProjection(const AProjection: IProjectionInfo): IProjectionInfo;
    function GetSuitableZoom(const AProjection: IProjectionInfo): Byte;
    function IsProjectionFromThisSet(const AProjection: IProjectionInfo): Boolean;
  public
    constructor Create(
      const AHash: THashValue;
      const AZooms: IInterfaceListStatic
    );
  end;

implementation

{ TProjectionSetSimple }

constructor TProjectionSetSimple.Create(
  const AHash: THashValue;
  const AZooms: IInterfaceListStatic
);
begin
  Assert(Assigned(AZooms));
  Assert(AZooms.Count > 0);
  inherited Create;
  FHash := AHash;
  FZooms := AZooms;
  FZoomCount := FZooms.Count;
end;

function TProjectionSetSimple.GetHash: THashValue;
begin
  Result := FHash;
end;

function TProjectionSetSimple.GetSuitableProjection(
  const AProjection: IProjectionInfo
): IProjectionInfo;
begin
  Result := IProjectionInfo(FZooms[AProjection.Zoom]); // TODO: fix later
end;

function TProjectionSetSimple.GetSuitableZoom(
  const AProjection: IProjectionInfo
): Byte;
begin
  Result := AProjection.Zoom; // TODO: fix later
end;

function TProjectionSetSimple.GetZoom(const AIndex: Byte): IProjectionInfo;
begin
  Result := IProjectionInfo(FZooms[AIndex]);
end;

function TProjectionSetSimple.GetZoomCount: Byte;
begin
  Result := FZoomCount;
end;

function TProjectionSetSimple.IsProjectionFromThisSet(
  const AProjection: IProjectionInfo
): Boolean;
var
  VZoom: Byte;
begin
  Assert(Assigned(AProjection));
  Result := False;
  VZoom := AProjection.Zoom;
  if VZoom < FZoomCount then begin
    // TODO: fix search zooms later
    Result := GetZoom(VZoom).GetIsSameProjectionInfo(AProjection);
  end;
end;

function TProjectionSetSimple.IsSame(
  const AProjectionSet: IProjectionSet
): Boolean;
var
  VSelf: IProjectionSet;
  i: Integer;
begin
  VSelf := Self;
  if VSelf = AProjectionSet then begin
    Result := True;
  end else if AProjectionSet = nil then begin
    Result := False;
  end else begin
    if (FHash <> 0) and (AProjectionSet.Hash <> 0) and (FHash <> AProjectionSet.Hash) then begin
      Result := False;
      Exit;
    end;
    Result := False;
    if FZoomCount = AProjectionSet.ZoomCount then begin
      Result := True;
      for i := 0 to FZoomCount - 1 do begin
        if not GetZoom(i).GetIsSameProjectionInfo(AProjectionSet.Zooms[i]) then begin
          Result := False;
          Break;
        end;
      end;
    end;
  end;
end;

function TProjectionSetSimple.CheckZoom(const AZoom: Byte): Boolean;
begin
  Result := AZoom < FZoomCount;
end;

procedure TProjectionSetSimple.ValidateZoom(var AZoom: Byte);
begin
  if AZoom >= FZoomCount then begin
    AZoom := FZoomCount - 1;
  end;
end;

end.
