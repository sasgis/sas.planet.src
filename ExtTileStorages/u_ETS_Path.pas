{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_ETS_Path;

interface

uses
  t_ETS_Path;
  
function ETS_TilePath_Divided(const AGlobalStorageIdentifier: String;
                              const AServiceName: String): TETS_Path_Divided;

function ETS_TilePath_Single(const AGlobalStorageIdentifier: String;
                             const AServiceName: String): String;

function ETS_TilePath_Divided_to_ServerDB(const pRec: PETS_Path_Divided): String;

procedure Clear_TilePath_Divided(pRec: PETS_Path_Divided);
procedure Copy_TilePath_Divided(const pSrc: PETS_Path_Divided; pDst: PETS_Path_Divided);

implementation

uses
  //Windows,
  SysUtils;

type
  TETS_Path_DelimPos = packed record
    pos_found: array [0..c_ETS_Path_Items_Count-1] of Integer; // 0 - no item
    count_found: Byte;
  end;
  PETS_Path_DelimPos = ^TETS_Path_DelimPos;

procedure Clear_TilePath_Divided(pRec: PETS_Path_Divided);
var i: Byte;
begin
  for i := 0 to c_ETS_Path_Items_Count-1 do
    pRec^.Path_Items[i]:='';
end;

procedure Copy_TilePath_Divided(const pSrc: PETS_Path_Divided; pDst: PETS_Path_Divided);
var i: Byte;
begin
  for i := 0 to c_ETS_Path_Items_Count-1 do
    pDst^.Path_Items[i]:=pSrc^.Path_Items[i];
end;

procedure InternalGet3Parts(const ASrc: String;
                            const p3items: PETS_Path_DelimPos;
                            const AMaxItemsToScan: Byte;
                            const AFromEnd: Boolean);
var
  i: Integer;
begin
  FillChar(p3items^, sizeof(p3items^), 0);
  // init
  if AFromEnd then
    i:=Length(ASrc)
  else
    i:=1;
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
      p3items^.pos_found[p3items^.count_found]:=i;
      Inc(p3items^.count_found);
      // found all requested items
      if (p3items^.count_found>=AMaxItemsToScan) then
        Exit;
    end;
    // next
    if AFromEnd then
      Dec(i)
    else
      Inc(i);
  until FALSE;
end;

function ETS_TilePath_Divided(const AGlobalStorageIdentifier: String;
                              const AServiceName: String): TETS_Path_Divided;
var
  t3pos: TETS_Path_DelimPos;
  i,result_items_defined: Byte;
  copy_count: Integer;
  s: String;
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
  result_items_defined:=0;
  Clear_TilePath_Divided(@Result);

  // parse AServiceName
  if (0<Length(AServiceName)) then begin
    // get from service string
    InternalGet3Parts(AServiceName, @t3pos, c_ETS_Path_Items_Count, TRUE);

    // add very first (2 delimiters - 3 parts)
    if (0<t3pos.count_found) then begin
      // add first text
      t3pos.pos_found[t3pos.count_found]:=0;
      Inc(t3pos.count_found);
    end;

    // loop through found parts
    if (0<t3pos.count_found) then
    for i := 0 to t3pos.count_found-1 do begin
      // extract string from start+1 to end-1 (start and end are markers of delimiters)
      if (0=i) then
        copy_count:=Length(AServiceName) // 'til the end
      else
        copy_count:=t3pos.pos_found[i-1]-t3pos.pos_found[i]-1;
      s:=System.Copy(AServiceName, t3pos.pos_found[i]+1, copy_count);
      Result.Path_Items[(c_ETS_Path_Items_Count-1)-result_items_defined]:=s;
      Inc(result_items_defined);
    end;
  end;

  // remains
  if (result_items_defined<c_ETS_Path_Items_Count) and (0<Length(AGlobalStorageIdentifier)) then begin
    // get from global string
    InternalGet3Parts(AGlobalStorageIdentifier, @t3pos, (c_ETS_Path_Items_Count-result_items_defined), FALSE);

    // add very last (0 delimiters - 1 part)
    if (0<t3pos.count_found) then begin
      // add last text
      t3pos.pos_found[t3pos.count_found]:=Length(AGlobalStorageIdentifier)+1;
      Inc(t3pos.count_found);
    end;

    // loop through found parts
    if (0<t3pos.count_found) then
    for i := 0 to t3pos.count_found-1 do begin
      // extract string from start+1 to end-1 (start and end are markers of delimiters)
      if (0=i) then
        copy_count:=0 // from the start
      else
        copy_count:=t3pos.pos_found[i-1];
      s:=System.Copy(AGlobalStorageIdentifier, copy_count+1, t3pos.pos_found[i]-copy_count-1);
      Result.Path_Items[i]:=s;
      Inc(result_items_defined);
      if (result_items_defined>=c_ETS_Path_Items_Count) then
        Exit;
    end;
  end;
end;

function ETS_TilePath_Single(const AGlobalStorageIdentifier: String;
                             const AServiceName: String): String;
begin
  with ETS_TilePath_Divided(AGlobalStorageIdentifier, AServiceName) do begin
    Result := Path_Items[0] + PathDelim + Path_Items[1] + PathDelim + Path_Items[2];
  end;
end;

function ETS_TilePath_Divided_to_ServerDB(const pRec: PETS_Path_Divided): String;
begin
  with pRec^ do begin
    Result := Path_Items[0];
    if (0<Length(Path_Items[1])) then
      Result := Result + PathDelim + Path_Items[1];
  end;
end;

end.
