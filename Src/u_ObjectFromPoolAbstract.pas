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

unit u_ObjectFromPoolAbstract;

interface

type
  TObjectFromPoolAbstract = class(TObject, IInterface)
  private
    FRefCount: Integer;
    FNextFree: TObjectFromPoolAbstract;
  protected
    function CheckNeedDestroyObject: Boolean; virtual; abstract;
    procedure InternalCleanup; virtual;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    property NextFree: TObjectFromPoolAbstract read FNextFree write FNextFree;
  end;

implementation

uses
  Windows;

{ TObjectFromPoolAbstract }

procedure TObjectFromPoolAbstract.InternalCleanup;
begin
  // Do nothing by default
end;

function TObjectFromPoolAbstract.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TObjectFromPoolAbstract._AddRef: Integer;
begin
  Assert(FNextFree = nil);
  Result := InterlockedIncrement(FRefCount);
end;

function TObjectFromPoolAbstract._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then begin
    InternalCleanup;
    if CheckNeedDestroyObject then begin
      Destroy;
    end;
  end;
end;

end.
