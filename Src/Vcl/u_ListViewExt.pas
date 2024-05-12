{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ListViewExt;

interface

uses
  ComCtrls;

type
  // TListView that can save/restore its columns width

  TListViewExt = class(TListView)
  private
    function GetColumnsWidth: string;
    procedure SetColumnsWidth(const AValue: string);
  public
    property ColumnsWidth: string read GetColumnsWidth write SetColumnsWidth;
  end;

  TListView = class(TListViewExt);

implementation

uses
  Classes,
  SysUtils;

{ TListViewExt }

function TListViewExt.GetColumnsWidth: string;
var
  I: Integer;
  VItems: array of string;
begin
  SetLength(VItems, Self.Columns.Count);
  for I := 0 to Self.Columns.Count - 1 do begin
    VItems[I] := IntToStr(Self.Columns[I].Width);
  end;
  Result := string.Join(',', VItems);
end;

procedure TListViewExt.SetColumnsWidth(const AValue: string);
var
  I: Integer;
  VCount: Integer;
  VItems: TArray<string>;
begin
  VItems := AValue.Split([',']);
  VCount := Length(VItems);
  if VCount = Self.Columns.Count then begin
    for I := 0 to VCount - 1 do begin
      Self.Columns[I].Width := StrToIntDef(VItems[I], Self.Columns[I].Width);
    end;
  end;
end;

end.
