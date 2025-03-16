{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit i_MarksExplorerFilter;

interface

uses
  i_Changeable,
  i_MarkDb,
  i_InterfaceListStatic;

type
  TSearchMethod = (smBegins, smContains, smMatch, smEnds, smRegEx);

  TMarksExplorerFilterConfig = record
    AllowPoints: Boolean;
    AllowPaths: Boolean;
    AllowPolygons: Boolean;

    SearchText: string;
    IgnoreCase: Boolean;
    SearchInName: Boolean;
    SearchInDesc: Boolean;
    SearchMethod: TSearchMethod;

    procedure Reset;
    function IsEnabled: Boolean;

    class operator Equal(const A, B: TMarksExplorerFilterConfig): Boolean;
    class operator NotEqual(const A, B: TMarksExplorerFilterConfig): Boolean; inline;
  end;

  IMarksExplorerFilter = interface(IChangeable)
    ['{64D7D750-3C60-4028-809D-5A75419AA073}']
    function GetEnabled: Boolean;
    procedure SetEnabled(const AValue: Boolean);
    property Enabled: Boolean read GetEnabled write SetEnabled;

    function GetConfig: TMarksExplorerFilterConfig;
    procedure SetConfig(const AValue: TMarksExplorerFilterConfig);
    property Config: TMarksExplorerFilterConfig read GetConfig write SetConfig;

    function Process(
      const AMarkDb: IMarkDb;
      const AMarkIdList: IInterfaceListStatic
    ): IInterfaceListStatic;
  end;

implementation

{ TMarksExplorerFilterConfig }

class operator TMarksExplorerFilterConfig.Equal(const A, B: TMarksExplorerFilterConfig): Boolean;
begin
  Result :=
    (A.AllowPoints   = B.AllowPoints)   and
    (A.AllowPaths    = B.AllowPaths)    and
    (A.AllowPolygons = B.AllowPolygons) and

    (A.SearchText    = B.SearchText)    and
    (A.IgnoreCase    = B.IgnoreCase)    and
    (A.SearchInName  = B.SearchInName)  and
    (A.SearchInDesc  = B.SearchInDesc)  and
    (A.SearchMethod  = B.SearchMethod);
end;

class operator TMarksExplorerFilterConfig.NotEqual(const A, B: TMarksExplorerFilterConfig): Boolean;
begin
  Result := not (A = B);
end;

procedure TMarksExplorerFilterConfig.Reset;
begin
  AllowPoints   := True;
  AllowPaths    := True;
  AllowPolygons := True;

  SearchText    := '';
  IgnoreCase    := True;
  SearchInName  := True;
  SearchInDesc  := False;
  SearchMethod  := smContains;
end;

function TMarksExplorerFilterConfig.IsEnabled: Boolean;
begin
  Result :=
    (SearchText <> '') or
    not AllowPoints or
    not AllowPaths or
    not AllowPolygons;
end;

end.
