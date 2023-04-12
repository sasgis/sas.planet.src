{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ContentTypeListByKey;

interface

uses
  ALStringList,
  u_AnsiStr,
  i_ContentTypeInfo;

type
  TContentTypeListByKey = class
  private
    FList: TStringListA;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(
      const AKey: AnsiString;
      const AType: IContentTypeInfoBasic
    );
    function Get(const AKey: AnsiString): IContentTypeInfoBasic;
    function GetEnumerator: TStringsEnumeratorA;
  end;


implementation

uses
  Classes,
  SysUtils;

{ TContentTypeListByKey }

procedure TContentTypeListByKey.Add(
  const AKey: AnsiString;
  const AType: IContentTypeInfoBasic
);
begin
  AType._AddRef;
  FList.AddObject(AKey, Pointer(AType));
end;

constructor TContentTypeListByKey.Create;
begin
  inherited Create;
  FList := TStringListA.Create;
  FList.Sorted := True;
  FList.Duplicates := dupError;
end;

destructor TContentTypeListByKey.Destroy;
var
  i: Integer;
begin
  if Assigned(FList) then begin
    for i := 0 to FList.Count - 1 do begin
      IInterface(Pointer(FList.Objects[i]))._Release;
    end;
    FreeAndNil(FList);
  end;
  inherited;
end;

function TContentTypeListByKey.Get(const AKey: AnsiString): IContentTypeInfoBasic;
var
  VIndex: Integer;
begin
  if FList.Find(AKey, VIndex) then begin
    Result := IContentTypeInfoBasic(Pointer(FList.Objects[VIndex]));
  end else begin
    Result := nil;
  end;
end;

function TContentTypeListByKey.GetEnumerator: TStringsEnumeratorA;
begin
  Result := FList.GetEnumerator;
end;

end.
