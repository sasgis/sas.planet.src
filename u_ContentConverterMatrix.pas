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

unit u_ContentConverterMatrix;

interface

uses
  ALStringList,
  i_ContentConverter;

type
  TContentConverterMatrix = class
  private
    FList: TALStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(
      const ASourceType, ATargetType: AnsiString;
      const AConverter: IContentConverter
    );
    function Get(const ASourceType, ATargetType: AnsiString): IContentConverter;
  end;

implementation

uses
  Classes,
  SysUtils,
  u_ContentConvertersListByKey;

{ TContentConverterMatrix }

procedure TContentConverterMatrix.Add(
  const ASourceType, ATargetType: AnsiString;
  const AConverter: IContentConverter
);
var
  VIndex: Integer;
  VList: TContentConvertersListByKey;
begin
  if FList.Find(ASourceType, VIndex) then begin
    VList := TContentConvertersListByKey(FList.Objects[VIndex]);
  end else begin
    VList := TContentConvertersListByKey.Create;
    FList.AddObject(ASourceType, VList);
  end;
  VList.Add(ATargetType, AConverter);
end;

constructor TContentConverterMatrix.Create;
begin
  inherited Create;
  FList := TALStringList.Create;
  FList.Sorted := True;
  FList.Duplicates := dupError;
end;

destructor TContentConverterMatrix.Destroy;
var
  i: Integer;
  VObj: TObject;
begin
  if Assigned(FList) then begin
    for i := 0 to FList.Count - 1 do begin
      VObj := FList.Objects[i];
      VObj.Free;
    end;
    FreeAndNil(FList);
  end;
  inherited;
end;

function TContentConverterMatrix.Get(
  const ASourceType, ATargetType: AnsiString
): IContentConverter;
var
  VIndex: Integer;
  VList: TContentConvertersListByKey;
begin
  if FList.Find(ASourceType, VIndex) then begin
    VList := TContentConvertersListByKey(FList.Objects[VIndex]);
    Result := VList.Get(ATargetType);
  end else begin
    Result := nil;
  end;
end;

end.
