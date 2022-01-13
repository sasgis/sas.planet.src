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

unit u_LanguageListStatic;

interface

uses
  Classes,
  i_LanguageListStatic,
  u_BaseInterfacedObject;

type
  TLanguageListStatic = class(TBaseInterfacedObject, ILanguageListStatic)
  private
    FSortedByCode: TStringList;
    FList: TStringList;
  private
    function GetCount: Integer;

    function GetCode(const AIndex: Integer): string;
    function FindCode(
      const ACode: string;
      out AIndex: Integer
    ): Boolean;
  public
    constructor Create(const AList: TStrings);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TLanguageListStatic }

constructor TLanguageListStatic.Create(const AList: TStrings);
var
  I: Integer;
  VCode: string;
begin
  inherited Create;
  FSortedByCode := TStringList.Create;
  FSortedByCode.Sorted := True;
  FSortedByCode.Duplicates := dupError;

  FList := TStringList.Create;

  for I := 0 to AList.Count - 1 do begin
    VCode := AList.Strings[I];
    FSortedByCode.AddObject(VCode, TObject(I));
    FList.Add(VCode);
  end;
end;

destructor TLanguageListStatic.Destroy;
begin
  FreeAndNil(FSortedByCode);
  FreeAndNil(FList);
  inherited;
end;

function TLanguageListStatic.FindCode(
  const ACode: string;
  out AIndex: Integer
): Boolean;
begin
  Result := FSortedByCode.Find(ACode, AIndex);
  if Result then begin
    AIndex := Integer(FSortedByCode.Objects[AIndex]);
  end;
end;

function TLanguageListStatic.GetCode(const AIndex: Integer): string;
begin
  Result := FList.Strings[AIndex];
end;

function TLanguageListStatic.GetCount: Integer;
begin
  Result := FList.Count;
end;

end.
