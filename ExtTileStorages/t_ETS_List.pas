{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit t_ETS_List;

interface

type
  T_ETS_ObjectList = record
    FObjects: array of TObject;
    // routines
    procedure SetZero;
    procedure AddItem(const AObj: TObject);
    procedure DelItem(const AObj: TObject);
    procedure FreeALL;
  end;

implementation

{ T_ETS_ObjectList }

procedure T_ETS_ObjectList.AddItem(const AObj: TObject);
var L: Integer;
begin
  L:=Length(FObjects);
  SetLength(FObjects,(L+1));
  FObjects[L]:=AObj;
end;

procedure T_ETS_ObjectList.FreeALL;
var
  i,L: Integer;
  obj: TObject;
begin
  L:=Length(FObjects);
  if (0<L) then begin
    // free items
    for i := L-1 downto 0 do begin
      obj:=FObjects[i];
      if (nil<>obj) then begin
        FObjects[i]:=nil;
        obj.Free;
      end;
    end;
    // shrink
    SetLength(FObjects,0);
  end;
end;

procedure T_ETS_ObjectList.DelItem(const AObj: TObject);
var i,L: Integer;
begin
  L:=Length(FObjects);
  if (0<L) then
  for i := L-1 downto 0 do
  if (AObj=FObjects[i]) then begin
    // cleanup link
    FObjects[i]:=nil;
    // compact
    if (i<(L-1)) then begin
      FObjects[i]:=FObjects[L-1];
      FObjects[L-1]:=nil;
    end;
    SetLength(FObjects, L-1);
    Exit;
  end;
end;

procedure T_ETS_ObjectList.SetZero;
begin
  SetLength(FObjects,0);
end;

end.