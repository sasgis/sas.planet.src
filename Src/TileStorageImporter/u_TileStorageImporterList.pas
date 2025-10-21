{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_TileStorageImporterList;

interface

uses
  Types,
  i_InterfaceListStatic,
  i_TileStorageImporter,
  i_TileStorageImporterList,
  u_BaseInterfacedObject;

type
  TTileStorageImporterListItem = class(TBaseInterfacedObject, ITileStorageImporterListItem)
  private
    FImporter: ITileStorageImporter;
    FSupportedExt: TStringDynArray;
    FName: string;
  private
    { ITileStorageImporterListItem }
    function GetImporter: ITileStorageImporter;
    function GetSupportedExt: TStringDynArray;
    function GetName: string;
  public
    constructor Create(
      const AImporter: ITileStorageImporter;
      const ASupportedExt: TStringDynArray;
      const AName: string
    );
  end;

  TTileStorageImporterListStatic = class(TBaseInterfacedObject, ITileStorageImporterListStatic)
  private
    FList: IInterfaceListStatic;
  private
    { ITileStorageImporterListStatic }
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): ITileStorageImporterListItem;
    function GetImporterByExt(const AExt: string): ITileStorageImporter;
  public
    constructor Create(const AList: IInterfaceListStatic);
  end;

implementation

uses
  SysUtils;

{ TTileStorageImporterListItem }

constructor TTileStorageImporterListItem.Create(
  const AImporter: ITileStorageImporter;
  const ASupportedExt: TStringDynArray;
  const AName: string
);
begin
  Assert(Assigned(AImporter));
  Assert(Length(ASupportedExt) > 0);
  Assert(AName <> '');

  inherited Create;

  FImporter := AImporter;
  FSupportedExt := ASupportedExt;
  FName := AName;
end;

function TTileStorageImporterListItem.GetSupportedExt: TStringDynArray;
begin
  Result := Copy(FSupportedExt);
end;

function TTileStorageImporterListItem.GetImporter: ITileStorageImporter;
begin
  Result := FImporter;
end;

function TTileStorageImporterListItem.GetName: string;
begin
  Result := FName;
end;

{ TTileStorageImporterListStatic }

constructor TTileStorageImporterListStatic.Create(
  const AList: IInterfaceListStatic
);
begin
  inherited Create;
  FList := AList;
end;

function TTileStorageImporterListStatic.GetCount: Integer;
begin
  if Assigned(FList) then begin
    Result := FList.Count;
  end else begin
    Result := 0;
  end;
end;

function TTileStorageImporterListStatic.GetImporterByExt(
  const AExt: string
): ITileStorageImporter;
var
  I, J: Integer;
  VExt: string;
  VExtArr: TStringDynArray;
  VItem: ITileStorageImporterListItem;
begin
  Result := nil;
  VExt := LowerCase(AExt);
  if VExt[1] = '.' then begin
    VExt := Copy(VExt, 2);
  end;
  for I := 0 to FList.Count - 1 do begin
    VItem := GetItem(I);
    VExtArr := TTileStorageImporterListItem(VItem).FSupportedExt;
    for J := 0 to Length(VExtArr) - 1 do begin
      if VExt = VExtArr[J] then begin
        Result := VItem.Importer;
        Break;
      end;
    end;
  end;
end;

function TTileStorageImporterListStatic.GetItem(
  const AIndex: Integer
): ITileStorageImporterListItem;
begin
  Result := ITileStorageImporterListItem(FList.Items[AIndex]);
end;

end.
