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
  ComCtrls;

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
    const AExpandInfo: TExpandInfo
  );

function GetSelectedNodeInfo(ATree: TTreeView): TExpandInfo;

function GetNodeUID(ANode: TTreeNode): Cardinal;

implementation

uses
  ALString,
  ALStringList,
  CRC32,
  i_MarkCategory;

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
  const AExpandInfo: TExpandInfo
);
var
  I, J, K: Integer;
  VUID: Cardinal;
begin
  J := ANodes.Count;
  for I := 0 to Length(AExpandInfo) - 1 do begin
    K := AExpandInfo[I].Index;
    if K < J then begin
      VUID := GetNodeUID(ANodes[K]);
      if VUID = AExpandInfo[I].UID then begin
        ANodes[K].Expand(False);
      end;
    end;
  end;
end;

function GetNodeUID(ANode: TTreeNode): Cardinal;
var
  VMarkCategory: IMarkCategory;
begin
  Result := 0;
  if ANode.Data <> nil then begin
    VMarkCategory := IMarkCategory(ANode.Data);
    if Assigned(VMarkCategory) then begin
      if VMarkCategory.Name <> '' then begin
        Result := CRC32Buf(PByte(@VMarkCategory.Name[1]), Length(VMarkCategory.Name) * SizeOf(Char));
      end;
    end;
  end;
end;

end.
