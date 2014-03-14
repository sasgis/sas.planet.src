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

unit u_BinaryDataByMemStream;

interface

uses
  Classes,
  i_BinaryData,
  u_BaseInterfacedObject;

type
  TBinaryDataByMemStream = class(TBaseInterfacedObject, IBinaryData)
  private
    FMemStream: TMemoryStream;
  private
    function GetBuffer: Pointer;
    function GetSize: Integer;
  public
    constructor CreateFromStream(AStream: TStream);
    constructor CreateWithOwn(var AMemStream: TMemoryStream);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TBinaryDataByMemStream }

constructor TBinaryDataByMemStream.CreateFromStream(AStream: TStream);
var
  VMemStream: TMemoryStream;
begin
  VMemStream := TMemoryStream.Create;
  try
    VMemStream.LoadFromStream(AStream);
    CreateWithOwn(VMemStream);
    VMemStream := nil;
  finally
    VMemStream.Free;
  end;
end;

constructor TBinaryDataByMemStream.CreateWithOwn(var AMemStream: TMemoryStream);
begin
  inherited Create;
  FMemStream := AMemStream;
  AMemStream := nil;
end;

destructor TBinaryDataByMemStream.Destroy;
begin
  FreeAndNil(FMemStream);
  inherited;
end;

function TBinaryDataByMemStream.GetBuffer: Pointer;
begin
  Result := FMemStream.Memory;
end;

function TBinaryDataByMemStream.GetSize: Integer;
begin
  Result := FMemStream.Size;
end;

end.
