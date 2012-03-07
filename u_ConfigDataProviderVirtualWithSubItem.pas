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

unit u_ConfigDataProviderVirtualWithSubItem;

interface

uses
  Classes,
  i_BinaryData,
  i_ConfigDataProvider;

type
  TConfigDataProviderVirtualWithSubItem = class(TInterfacedObject, IConfigDataProvider)
  private
    FSubItemName: string;
    FSubItem: IConfigDataProvider;
  protected
    function GetSubItem(const AIdent: string): IConfigDataProvider; virtual;
    function ReadBinary(const AIdent: string): IBinaryData; virtual;
    function ReadString(const AIdent: string; const ADefault: string): string; virtual;
    function ReadInteger(const AIdent: string; const ADefault: Longint): Longint; virtual;
    function ReadBool(const AIdent: string; const ADefault: Boolean): Boolean; virtual;
    function ReadDate(const AIdent: string; const ADefault: TDateTime): TDateTime; virtual;
    function ReadDateTime(const AIdent: string; const ADefault: TDateTime): TDateTime; virtual;
    function ReadFloat(const AIdent: string; const ADefault: Double): Double; virtual;
    function ReadTime(const AIdent: string; const ADefault: TDateTime): TDateTime; virtual;

    procedure ReadSubItemsList(AList: TStrings); virtual;
    procedure ReadValuesList(AList: TStrings); virtual;
  public
    constructor Create(
      ASubItemName: string;
      ASubItem: IConfigDataProvider
    );
    destructor Destroy; override;
  end;

implementation

{ TConfigDataProviderVirtualWithSubItem }

constructor TConfigDataProviderVirtualWithSubItem.Create(ASubItemName: string;
  ASubItem: IConfigDataProvider);
begin
  FSubItemName := ASubItemName;
  FSubItem := ASubItem;
end;

destructor TConfigDataProviderVirtualWithSubItem.Destroy;
begin
  FSubItem := nil;
  inherited;
end;

function TConfigDataProviderVirtualWithSubItem.GetSubItem(
  const AIdent: string): IConfigDataProvider;
begin
  if AIdent = FSubItemName then begin
    Result := FSubItem;
  end else begin
    Result := nil;
  end;
end;

function TConfigDataProviderVirtualWithSubItem.ReadBinary(const AIdent: string): IBinaryData;
begin
  Result := nil;
end;

function TConfigDataProviderVirtualWithSubItem.ReadBool(const AIdent: string;
  const ADefault: Boolean): Boolean;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadDate(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadDateTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadFloat(const AIdent: string;
  const ADefault: Double): Double;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadInteger(const AIdent: string;
  const ADefault: Integer): Longint;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadString(const AIdent,
  ADefault: string): string;
begin
  Result := ADefault;
end;

procedure TConfigDataProviderVirtualWithSubItem.ReadSubItemsList(AList: TStrings);
begin
  AList.Clear;
  AList.Add(FSubItemName);
end;

function TConfigDataProviderVirtualWithSubItem.ReadTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

procedure TConfigDataProviderVirtualWithSubItem.ReadValuesList(AList: TStrings);
begin
  AList.Clear;
end;

end.
