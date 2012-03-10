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

unit u_ConfigDataProviderByResources;

interface

uses
  Types,
  Classes,
  i_StringListStatic,
  i_BinaryData,
  i_ConfigDataProvider;

type
  TConfigDataProviderByResources = class(TInterfacedObject, IConfigDataProvider)
  private
    FInstance: THandle;
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

    function ReadSubItemsList: IStringListStatic;
    function ReadValuesList: IStringListStatic;
  public
    constructor Create(AInstance: THandle);
  end;

implementation

uses
  SysUtils,
  u_StringListStatic,
  u_BinaryDataByMemStream;

{ TConfigDataProviderByResources }

constructor TConfigDataProviderByResources.Create(AInstance: THandle);
begin
  FInstance := AInstance;
end;

function TConfigDataProviderByResources.GetSubItem(
  const AIdent: string): IConfigDataProvider;
begin
  Result := nil;
end;

function TConfigDataProviderByResources.ReadBinary(const AIdent: string): IBinaryData;
var
  VResStream: TResourceStream;
  VMemStream: TMemoryStream;
begin
  try
    VResStream := TResourceStream.Create(HInstance, ChangeFileExt(AIdent, ''), RT_RCDATA);
    try
      VMemStream := TMemoryStream.Create;
      try
        VResStream.SaveToStream(VMemStream);
      except
        VMemStream.Free;
        raise;
      end;
      Result := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
    finally
      VResStream.Free;
    end;
  except
    Result := nil;
  end;
end;

function TConfigDataProviderByResources.ReadBool(const AIdent: string;
  const ADefault: Boolean): Boolean;
begin
  Result := ADefault;
end;

function TConfigDataProviderByResources.ReadDate(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByResources.ReadDateTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByResources.ReadFloat(const AIdent: string;
  const ADefault: Double): Double;
begin
  Result := ADefault;
end;

function TConfigDataProviderByResources.ReadInteger(const AIdent: string;
  const ADefault: Integer): Longint;
begin
  Result := ADefault;
end;

function TConfigDataProviderByResources.ReadString(const AIdent,
  ADefault: string): string;
begin
  Result := ADefault;
end;

function TConfigDataProviderByResources.ReadSubItemsList: IStringListStatic;
var
  VList: TStringList;
begin
  VList := TStringList.Create;
  Result := TStringListStatic.CreateWithOwn(VList);
end;

function TConfigDataProviderByResources.ReadTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByResources.ReadValuesList: IStringListStatic;
var
  VList: TStringList;
begin
  VList := TStringList.Create;
  Result := TStringListStatic.CreateWithOwn(VList);
end;

end.
