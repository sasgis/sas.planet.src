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

unit u_MarksExplorerFilter;

interface

uses
  i_MarkDb,
  i_MarksExplorerFilter,
  i_InterfaceListStatic,
  u_ChangeableBase;

type
  TMarksExplorerFilter = class(TChangeableBase, IMarksExplorerFilter)
  private
    FEnabled: Boolean;
    FConfig: TMarksExplorerFilterConfig;
  private
    { IMarksExplorerFilter }
    function GetEnabled: Boolean;
    procedure SetEnabled(const AValue: Boolean);

    function GetConfig: TMarksExplorerFilterConfig;
    procedure SetConfig(const AValue: TMarksExplorerFilterConfig);

    function Process(
      const AMarkDb: IMarkDb;
      const AMarkIdList: IInterfaceListStatic
    ): IInterfaceListStatic;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  RegularExpressions,
  i_MarkId,
  i_InterfaceListSimple,
  i_VectorDataItemSimple,
  u_InterfaceListSimple,
  u_Synchronizer;

{ TMarksExplorerFilter }

constructor TMarksExplorerFilter.Create;
begin
  inherited Create(GSync.SyncStd.Make(Self.ClassName));
  FEnabled := False;
  FConfig.Reset;
end;

function TMarksExplorerFilter.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TMarksExplorerFilter.SetEnabled(const AValue: Boolean);
begin
  if AValue <> FEnabled then begin
    FEnabled := AValue and FConfig.IsEnabled;
    Self.DoChangeNotify;
  end;
end;

function TMarksExplorerFilter.GetConfig: TMarksExplorerFilterConfig;
begin
  Result := FConfig;
end;

procedure TMarksExplorerFilter.SetConfig(const AValue: TMarksExplorerFilterConfig);
begin
  if AValue <> FConfig then begin
    FConfig := AValue;
    FEnabled := FEnabled and FConfig.IsEnabled;
    Self.DoChangeNotify;
  end;
end;

function TMarksExplorerFilter.Process(
  const AMarkDb: IMarkDb;
  const AMarkIdList: IInterfaceListStatic
): IInterfaceListStatic;
var
  I: Integer;
  VDoAdd: Boolean;
  VMarkId: IMarkId;
  VMark: IVectorDataItem;
  VName, VDesc, VText: string;
  VRegEx: TRegEx;
  VFilteredList: IInterfaceListSimple;
begin
  if not FEnabled or (AMarkIdList = nil) then begin
    Result := AMarkIdList;
    Exit;
  end;

  VFilteredList := TInterfaceListSimple.Create;

  VText := FConfig.SearchText;
  if (VText <> '') and FConfig.IgnoreCase then begin
    VText := AnsiLowerCase(VText);
  end;

  if (VText <> '') and (FConfig.SearchMethod = smRegEx) then begin
    VRegEx := TRegEx.Create(VText, [roNotEmpty, roCompiled]); // record initialization
  end;

  for I := 0 to AMarkIdList.Count - 1 do begin
    VMarkId := IMarkId(AMarkIdList.Items[I]);

    // filter by MarkType
    case VMarkId.MarkType of
      midPoint: VDoAdd := FConfig.AllowPoints;
      midLine:  VDoAdd := FConfig.AllowPaths;
      midPoly:  VDoAdd := FConfig.AllowPolygons;
    else
      VDoAdd := False;
      Assert(False);
    end;

    if not VDoAdd then begin
      Continue;
    end;

    // filter by Name/Description Text
    if VText <> '' then begin

      // prepare Name
      if FConfig.SearchInName then begin
        VName := VMarkId.Name;
        if (VName <> '') and FConfig.IgnoreCase then begin
          VName := AnsiLowerCase(VName);
        end;
      end else begin
        VName := '';
      end;

      // prepare Description
      if FConfig.SearchInDesc then begin
        VMark := AMarkDb.GetMarkByID(VMarkId);
        if VMark <> nil then begin
          VDesc := VMark.Desc;
        end else begin
          VDesc := '';
        end;
        if (VDesc <> '') and FConfig.IgnoreCase then begin
          VDesc := AnsiLowerCase(VDesc);
        end;
      end else begin
        VDesc := '';
      end;

      // do search
      case FConfig.SearchMethod of
        smBegins:   VDoAdd := VName.StartsWith(VText) or VDesc.StartsWith(VText);
        smContains: VDoAdd := (Pos(VText, VName) > 0) or (Pos(VText, VDesc) > 0);
        smMatch:    VDoAdd := (VText = VName) or (VText = VDesc);
        smEnds:     VDoAdd := VName.EndsWith(VText) or VDesc.EndsWith(VText);
        smRegEx:    VDoAdd := ((VName <> '') and VRegEx.IsMatch(VName)) or ((VDesc <> '') and VRegEx.IsMatch(VDesc));
      else
        VDoAdd := False;
        Assert(False);
      end;
    end;

    if VDoAdd then begin
      VFilteredList.Add(VMarkId);
    end;
  end;

  Result := VFilteredList.MakeStaticAndClear;
end;

end.
