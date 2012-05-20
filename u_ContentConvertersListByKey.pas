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

unit u_ContentConvertersListByKey;

interface

uses
  Classes,
  i_ContentConverter;

type
  TContentConvertersListByKey = class
  private
    FList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(
      const AKey: string;
      const AConverter: IContentConverter
    );
    function Get(const AKey: string): IContentConverter;
  end;

implementation

uses
  SysUtils;

{ TContentConvertersListByKey }

procedure TContentConvertersListByKey.Add(
  const AKey: string;
  const AConverter: IContentConverter
);
begin
  AConverter._AddRef;
  FList.AddObject(AKey, Pointer(AConverter));
end;

constructor TContentConvertersListByKey.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  FList.Sorted := True;
  FList.Duplicates := dupError;
end;

destructor TContentConvertersListByKey.Destroy;
var
  i: Integer;
begin
  if FList <> nil then begin
    for i := 0 to FList.Count - 1 do begin
      IInterface(Pointer(FList.Objects[i]))._Release;
    end;
    FreeAndNil(FList);
  end;
  inherited;
end;

function TContentConvertersListByKey.Get(const AKey: string): IContentConverter;
var
  VIndex: Integer;
begin
  if FList.Find(AKey, VIndex) then begin
    Result := IContentConverter(Pointer(FList.Objects[VIndex]));
  end else begin
    Result := nil;
  end;
end;

end.
