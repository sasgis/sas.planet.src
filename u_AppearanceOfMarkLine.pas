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

unit u_AppearanceOfMarkLine;

interface

uses
  GR32,
  t_Hash,
  i_Appearance,
  i_AppearanceOfVectorItem,
  u_BaseInterfacedObject;

type
  TAppearanceOfMarkLine = class(TBaseInterfacedObject, IAppearance, IAppearanceLine)
  private
    FHash: THashValue;
    FLineColor: TColor32;
    FLineWidth: Integer;
  private { IAppearance }
    function GetHash: THashValue;
    function IsEqual(const AValue: IAppearance): Boolean;
  private { IAppearanceLine }
    function GetLineColor: TColor32;
    function GetLineWidth: Integer;
  public
    constructor Create(
      const AHash: THashValue;
      const ALineColor: TColor32;
      const ALineWidth: Integer
    );
  end;

implementation

{ TAppearanceOfMarkLine }

constructor TAppearanceOfMarkLine.Create(
  const AHash: THashValue;
  const ALineColor: TColor32;
  const ALineWidth: Integer
);
begin
  inherited Create;
  FHash := AHash;
  FLineColor := ALineColor;
  FLineWidth := ALineWidth;
end;

function TAppearanceOfMarkLine.GetHash: THashValue;
begin
  Result := FHash;
end;

function TAppearanceOfMarkLine.GetLineColor: TColor32;
begin
  Result := FLineColor;
end;

function TAppearanceOfMarkLine.GetLineWidth: Integer;
begin
  Result := FLineWidth;
end;

function TAppearanceOfMarkLine.IsEqual(const AValue: IAppearance): Boolean;
begin
  if not Assigned(AValue) then begin
    Result := False;
  end else begin
    Result := FHash = AValue.Hash;
  end;
end;

end.
