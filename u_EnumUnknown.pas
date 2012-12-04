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

unit u_EnumUnknown;

interface

uses
  Classes,
  ActiveX,
  u_BaseInterfacedObject;

type
  TEnumUnknown = class(TBaseInterfacedObject, IEnumUnknown)
  private
    FList: IInterfaceList;
    FCurrent: Longint;
    function Next(
      celt: Longint;
      out elt;
      pceltFetched: PLongint
    ): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumUnknown): HResult; stdcall;
  public
    constructor Create(AList: IInterfaceList);
    destructor Destroy; override;
  end;

  TEnumUnknownTwoItems = class(TBaseInterfacedObject, IEnumUnknown)
  private
    FItem1: IInterface;
    FItem2: IInterface;
    FCurrent: Longint;
  private
    function Next(
      celt: Longint;
      out elt;
      pceltFetched: PLongint
    ): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumUnknown): HResult; stdcall;
  public
    constructor Create(
      const AItem1: IInterface;
      const AItem2: IInterface
    );
  end;


implementation

{ TEnumUnknown }

function TEnumUnknown.Clone(out enm: IEnumUnknown): HResult;
var
  VClone: TEnumUnknown;
begin
  VClone := TEnumUnknown.Create(FList);
  VClone.FCurrent := FCurrent;
  enm := VClone;
  Result := S_OK;
end;

constructor TEnumUnknown.Create(AList: IInterfaceList);
begin
  inherited Create;
  FList := AList;
  FCurrent := 0;
end;

destructor TEnumUnknown.Destroy;
begin
  FList := nil;
  inherited;
end;

function TEnumUnknown.Next(
  celt: Integer;
  out elt;
  pceltFetched: PLongint
): HResult;
begin
  if celt <> 1 then begin
    Result := E_NOTIMPL;
  end else begin
    IInterface(elt) := nil;
    FList.Lock;
    try
      if FCurrent < FList.Count then begin
        IInterface(elt) := FList.Items[FCurrent];
        Inc(FCurrent);
        if pceltFetched <> nil then begin
          pceltFetched^ := 1;
        end;
        Result := S_OK;
      end else begin
        if pceltFetched <> nil then begin
          pceltFetched^ := 0;
        end;
        Result := S_FALSE;
      end;
    finally
      FList.Unlock;
    end;
  end;
end;

function TEnumUnknown.Reset: HResult;
begin
  FCurrent := 0;
  Result := S_OK;
end;

function TEnumUnknown.Skip(celt: Integer): HResult;
begin
  FList.Lock;
  try
    if FCurrent + celt <= FList.Count then begin
      Inc(FCurrent, celt);
      Result := S_OK;
    end else begin
      FCurrent := FList.Count;
      Result := S_FALSE;
    end;
  finally
    FList.Unlock;
  end;
end;

{ TEnumUnknownTwoItems }

constructor TEnumUnknownTwoItems.Create(const AItem1, AItem2: IInterface);
begin
  inherited Create;
  FItem1 := AItem1;
  FItem2 := AItem2;
  FCurrent := 0;
end;

function TEnumUnknownTwoItems.Clone(out enm: IEnumUnknown): HResult;
var
  VClone: TEnumUnknownTwoItems;
begin
  VClone := TEnumUnknownTwoItems.Create(FItem1, FItem2);
  VClone.FCurrent := FCurrent;
  enm := VClone;
  Result := S_OK;
end;

function TEnumUnknownTwoItems.Next(celt: Integer; out elt;
  pceltFetched: PLongint): HResult;
begin
  if celt <> 1 then begin
    Result := E_NOTIMPL;
  end else begin
    IInterface(elt) := nil;
    if FCurrent >= 2 then begin
      if pceltFetched <> nil then begin
        pceltFetched^ := 0;
      end;
      Result := S_FALSE;
    end else if FCurrent = 1 then begin
      IInterface(elt) := FItem2;
      Inc(FCurrent);
      if pceltFetched <> nil then begin
        pceltFetched^ := 1;
      end;
      Result := S_OK;
    end else begin
      IInterface(elt) := FItem1;
      Inc(FCurrent);
      if pceltFetched <> nil then begin
        pceltFetched^ := 1;
      end;
      Result := S_OK;
    end;
  end;
end;

function TEnumUnknownTwoItems.Reset: HResult;
begin
  FCurrent := 0;
  Result := S_OK;
end;

function TEnumUnknownTwoItems.Skip(celt: Integer): HResult;
begin
  if FCurrent + celt <= 2 then begin
    Inc(FCurrent, celt);
    Result := S_OK;
  end else begin
    FCurrent := 2;
    Result := S_FALSE;
  end;
end;

end.
