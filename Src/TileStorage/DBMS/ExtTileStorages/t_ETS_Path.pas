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

unit t_ETS_Path;

interface

const
  c_ETS_Path_Items_Count = 3;

type
  PETS_Path_Divided_W = ^TETS_Path_Divided_W;
  TETS_Path_Divided_W = record
    Path_Items: array [0..c_ETS_Path_Items_Count - 1] of WideString;
    // 0 - ServerName: WideString;
    // 1 - DatabaseName: WideString;
    // 2 - TableName: WideString;
  public
    procedure Clear;
    procedure CopyFrom(const ASrc: PETS_Path_Divided_W);
    // fill from sas connection params
    procedure ApplyFrom(const AGlobalStorageIdentifier, AServiceName: WideString);
    // get servername\databasename as endpoint
    function AsEndpoint: WideString;
    function AsWideString: WideString;
    // parts
    function ServerName: WideString; inline;
    function ServiceName: WideString; inline;
  end;

implementation

uses
  SysUtils;

type
  TETS_Path_DelimPos = packed record
    pos_found: array [0..c_ETS_Path_Items_Count - 1] of Integer; // 0 - no item
    count_found: Byte;
  private
    procedure Get3Parts(
      const ASrc: WideString;
      const AMaxItemsToScan: Byte;
      const AFromEnd: Boolean
    );
  end;
  PETS_Path_DelimPos = ^TETS_Path_DelimPos;

{ TETS_Path_Divided_W }

procedure TETS_Path_Divided_W.ApplyFrom(const AGlobalStorageIdentifier, AServiceName: WideString);
var
  t3pos: TETS_Path_DelimPos;
  i, result_items_defined: Byte;
  copy_count: Integer;
  s: WideString;
begin
  // AServiceName - set for single map service
  // allowed:
  // A1) 'yasat'
  // A2) 'sas_database\dgsat'
  // A3) 'sas_server\sas_database\sas_service_table'
  // A4) 'text\sas_server\sas_database\sas_service_table'
  // if need to define special (schema) table owner - use 'dbo.table' notation

  // AGlobalStorageIdentifier - set for entire EXE
  // allowed:
  // B1) 'sas_cache'
  // B2) 'sas_cache\sas_cache'
  // B3) ''

  // a) Divide params by PathDelim
  // b) get up to 3 items from AServiceName (from end to begin)
  // c) if got less then 3 items - get remains from AGlobalStorageIdentifier (from start)

  result_items_defined := 0;
  Self.Clear;

  // parse AServiceName
  if (0<Length(AServiceName)) then begin
    // get from service string
    t3pos.Get3Parts(AServiceName, c_ETS_Path_Items_Count, TRUE);

    // add very first (2 delimiters - 3 parts)
    //if (0<=t3pos.count_found) then begin
      // add first text
      t3pos.pos_found[t3pos.count_found] := 0;
      Inc(t3pos.count_found);
    //end;

    // loop through found parts
    if (0<t3pos.count_found) then
    for i := 0 to t3pos.count_found - 1 do begin
      // extract string from start+1 to end-1 (start and end are markers of delimiters)
      if (0=i) then
        copy_count := Length(AServiceName) // 'til the end
      else
        copy_count := t3pos.pos_found[i - 1] - t3pos.pos_found[i] - 1;
      s := System.Copy(AServiceName, t3pos.pos_found[i] + 1, copy_count);
      Self.Path_Items[(c_ETS_Path_Items_Count - 1) - result_items_defined] := s;
      Inc(result_items_defined);
    end;
  end;

  // remains
  if (result_items_defined<c_ETS_Path_Items_Count) and (0<Length(AGlobalStorageIdentifier)) then begin
    // get from global string
    t3pos.Get3Parts(AGlobalStorageIdentifier, (c_ETS_Path_Items_Count - result_items_defined), FALSE);

    // add very last (0 delimiters - 1 part)
    if (0<t3pos.count_found) then begin
      // add last text
      t3pos.pos_found[t3pos.count_found] := Length(AGlobalStorageIdentifier) + 1;
      Inc(t3pos.count_found);
    end;

    // loop through found parts
    if (0<t3pos.count_found) then
    for i := 0 to t3pos.count_found - 1 do begin
      // extract string from start+1 to end-1 (start and end are markers of delimiters)
      if (0=i) then
        copy_count := 0 // from the start
      else
        copy_count := t3pos.pos_found[i - 1];
      s := System.Copy(AGlobalStorageIdentifier, copy_count + 1, t3pos.pos_found[i] - copy_count - 1);
      Self.Path_Items[i] := s;
      Inc(result_items_defined);
      if (result_items_defined>=c_ETS_Path_Items_Count) then
        Exit;
    end;
  end;
end;

function TETS_Path_Divided_W.AsEndpoint: WideString;
begin
  Result := Path_Items[0];
  if (0<Length(Path_Items[1])) then
    Result := Result + PathDelim + Path_Items[1];
end;

function TETS_Path_Divided_W.AsWideString: WideString;
begin
  Result := Path_Items[0] + PathDelim + Path_Items[1] + PathDelim + Path_Items[2];
end;

procedure TETS_Path_Divided_W.Clear;
var i: Byte;
begin
  for i := 0 to c_ETS_Path_Items_Count - 1 do
    Path_Items[i]:='';
end;

procedure TETS_Path_Divided_W.CopyFrom(const ASrc: PETS_Path_Divided_W);
var i: Byte;
begin
  if (nil=ASrc) then begin
    Clear;
    Exit;
  end;

  for i := 0 to c_ETS_Path_Items_Count - 1 do
    Path_Items[i] := ASrc^.Path_Items[i];
end;

function TETS_Path_Divided_W.ServerName: WideString;
begin
  Result := Path_Items[0]
end;

function TETS_Path_Divided_W.ServiceName: WideString;
begin
  Result := Path_Items[2]
end;

{ TETS_Path_DelimPos }

procedure TETS_Path_DelimPos.Get3Parts(
  const ASrc: WideString;
  const AMaxItemsToScan: Byte;
  const AFromEnd: Boolean
);
var
  i: Integer;
begin
  FillChar(Self, sizeof(Self), 0);
  // init
  if AFromEnd then
    i := Length(ASrc)
  else
    i := 1;
  // loop
  repeat
    // check bounds
    if (i<=0) then
      Exit;
    if (i>Length(ASrc)) then
      Exit;
    // work
    if (PathDelim=ASrc[i]) then begin
      // save positions of delimiter
      Self.pos_found[Self.count_found] := i;
      Inc(Self.count_found);
      // found all requested items
      if (Self.count_found >= AMaxItemsToScan) then
        Exit;
    end;
    // next
    if AFromEnd then
      Dec(i)
    else
      Inc(i);
  until FALSE;
end;

end.