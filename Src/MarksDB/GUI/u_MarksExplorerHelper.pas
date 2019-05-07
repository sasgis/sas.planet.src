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

unit u_MarksExplorerHelper;

interface

uses
  Classes,
  ComCtrls,
  TBX,
  TB2Item,
  CityHash,
  i_Category,
  i_MarkSystemConfig;

type
  TExpandInfoItem = record
    UID: Cardinal;
    Index: Integer;
  end;
  TExpandInfo = array of TExpandInfoItem;

function ExpandInfoToString(const AInfo: TExpandInfo): AnsiString;
function ExpandInfoFromString(const AStr: AnsiString): TExpandInfo;

function GetExpandInfo(ANodes: TTreeNodes): TExpandInfo;

procedure DoExpandNodes(
  ANodes: TTreeNodes;
  const AExpandInfo: TExpandInfo;
  const ASelected: ICategory
);

function GetSelectedNodeInfo(ATree: TTreeView): TExpandInfo;

function GetNodeUID(ANode: TTreeNode): Cardinal; inline;

procedure RefreshConfigListMenu(
  const ASubmenuItem: TTBXSubmenuItem;
  const ARefreshCaption: Boolean;
  const AOnClick: TNotifyEvent;
  const AMarkSystemConfig: IMarkSystemConfigListChangeable
);

implementation

uses
  SysUtils,
  ALString,
  ALStringList,
  c_MarkSystem,
  i_InterfaceListStatic;

const
  cSep1: AnsiChar = ',';
  cSep2: AnsiChar = ':';

function ExpandInfoToString(const AInfo: TExpandInfo): AnsiString;
var
  I: Integer;
  VStrings: TALStringList;
begin
  VStrings := TALStringList.Create;
  try
    VStrings.Delimiter := cSep1;
    VStrings.NameValueSeparator := cSep2;
    for I := 0 to Length(AInfo) - 1 do begin
      VStrings.Values[ALIntToHex(AInfo[I].UID, 8)] := ALIntToStr(AInfo[I].Index);
    end;
    Result := VStrings.DelimitedText;
  finally
    VStrings.Free;
  end;
end;

function ExpandInfoFromString(const AStr: AnsiString): TExpandInfo;
var
  I: Integer;
  VStrings: TALStringList;
begin
  if AStr <> '' then begin
    VStrings := TALStringList.Create;
    try
      VStrings.Delimiter := cSep1;
      VStrings.NameValueSeparator := cSep2;
      VStrings.DelimitedText := AStr;
      SetLength(Result, VStrings.Count);
      for I := 0 to Length(Result) - 1 do begin
        Result[I].UID := ALStrToInt('$' + VStrings.Names[I]);
        Result[I].Index := ALStrToInt(VStrings.ValueFromIndex[I]);
      end;
    finally
      VStrings.Free;
    end;
  end else begin
    SetLength(Result, 0);
  end;
end;

function GetSelectedNodeInfo(ATree: TTreeView): TExpandInfo;
var
  VNode: TTreeNode;
begin
  VNode := ATree.Selected;
  if VNode <> nil then begin
    SetLength(Result, 1);
    Result[0].Index := VNode.AbsoluteIndex;
    Result[0].UID := GetNodeUID(VNode);
  end else begin
    SetLength(Result, 0);
  end;
end;

function GetExpandInfo(ANodes: TTreeNodes): TExpandInfo;
var
  I, J: Integer;
  VNode: TTreeNode;
begin
  J := 0;
  SetLength(Result, 64);
  for I := 0 to ANodes.Count - 1 do begin
    VNode := ANodes[I];
    if (VNode <> nil) and VNode.Expanded then begin
      if J >= Length(Result) then begin
        SetLength(Result, Length(Result) * 2);
      end;
      Result[J].UID := GetNodeUID(VNode);
      Result[J].Index := I;
      Inc(J);
    end;
  end;
  SetLength(Result, J);
end;

procedure DoExpandNodes(
  ANodes: TTreeNodes;
  const AExpandInfo: TExpandInfo;
  const ASelected: ICategory
);
var
  I, J: Integer;
  VCount: Integer;
  VSelected: Integer;
  VNode: TTreeNode;
begin
  VSelected := -1;
  VCount := ANodes.Count;

  if ASelected <> nil then begin
    for I := 0 to VCount - 1 do begin
      VNode := ANodes[I];
      if
        (VNode <> nil) and
        (VNode.Data <> nil) and
        ASelected.IsSame(ICategory(VNode.Data)) then
      begin
        VSelected := I;
        Break;
      end;
    end;
  end;

  for I := 0 to VCount - 1 do begin
    ANodes[I].Collapse(False);
  end;

  for I := 0 to Length(AExpandInfo) - 1 do begin
    J := AExpandInfo[I].Index;
    if J < VCount then begin
      VNode := ANodes[J];
      if GetNodeUID(VNode) = AExpandInfo[I].UID then begin
        VNode.Expand(False);
      end;
    end;
  end;

  if VSelected >= 0 then begin
    ANodes[VSelected].Selected := True;
  end;
end;

function GetNodeUID(ANode: TTreeNode): Cardinal;
var
  VCategory: ICategory;
begin
  Result := 0;
  if (ANode <> nil) and (ANode.Data <> nil) then begin
    VCategory := ICategory(ANode.Data);
    if Assigned(VCategory) and (VCategory.Name <> '') then begin
      Result := CityHash32(@VCategory.Name[1], Length(VCategory.Name) * SizeOf(Char));
    end;
  end;
end;

procedure RefreshConfigListMenu(
  const ASubmenuItem: TTBXSubmenuItem;
  const ARefreshCaption: Boolean;
  const AOnClick: TNotifyEvent;
  const AMarkSystemConfig: IMarkSystemConfigListChangeable
);

  function _DatabaseToHint(const ADB: TGUID): string;
  begin
    if IsEqualGUID(ADB, cSMLMarksDbGUID) then begin
      Result := rsSMLMarksDbName;
    end else if IsEqualGUID(ADB, cORMSQLiteMarksDbGUID) then begin
      Result := rsORMSQLiteMarksDbName;
    end else if IsEqualGUID(ADB, cORMMongoDbMarksDbGUID) then begin
      Result := rsORMMongoDbMarksDbName;
    end else if IsEqualGUID(ADB, cORMODBCMarksDbGUID) then begin
      Result := rsORMODBCMarksDbName;
    end else if IsEqualGUID(ADB, cORMZDBCMarksDbGUID) then begin
      Result := rsORMZDBCMarksDbName;
    end else begin
      Result := '';
      Exit;
    end;
    Result := Format(' [%s]', [Result]);
  end;

var
  I: Integer;
  VList: IInterfaceListStatic;
  VActiveID: Integer;
  VMenuItem: TTBXItem;
  VConfigItem: IMarkSystemConfigStatic;
begin
  ASubmenuItem.Clear;

  if ARefreshCaption then begin
    ASubmenuItem.Caption := '';
  end;

  VList := AMarkSystemConfig.GetIDListStatic;
  if Assigned(VList) and (VList.Count > 0) then begin
    ASubmenuItem.Enabled := True;
    ASubmenuItem.Options := ASubmenuItem.Options + [tboDropdownArrow];
  end else begin
    ASubmenuItem.Enabled := False;
    ASubmenuItem.Options := ASubmenuItem.Options - [tboDropdownArrow];
    Exit;
  end;

  VActiveID := AMarkSystemConfig.ActiveConfigID;

  for I := 0 to VList.Count - 1 do begin
    VConfigItem := IMarkSystemConfigStatic(VList.Items[I]);
    VMenuItem := TTBXItem.Create(ASubmenuItem);
    VMenuItem.Tag := VConfigItem.ID;
    VMenuItem.Caption := VConfigItem.DisplayName + _DatabaseToHint(VConfigItem.DatabaseGUID);
    VMenuItem.Checked := (VConfigItem.ID = VActiveID);
    VMenuItem.RadioItem := True;
    VMenuItem.GroupIndex := 1;
    VMenuItem.OnClick := AOnClick;

    ASubmenuItem.Add(VMenuItem);

    if VMenuItem.Checked and ARefreshCaption then begin
      ASubmenuItem.Caption := VMenuItem.Caption;
    end;
  end;
end;

end.
