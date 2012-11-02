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

unit u_InternalDomainInfoProviderByMarksSystem;

interface

uses
  i_BinaryData,
  i_MarksSystem,
  i_InternalDomainInfoProvider;

type
  TInternalDomainInfoProviderByMarksSystem = class(TInterfacedObject, IInternalDomainInfoProvider)
  private
    FMarksSystem: IMarksSystem;
  private
    function LoadBinaryByFilePath(
      const AFilePath: string;
      out AContentType: string
    ): IBinaryData;
  public
    constructor Create(
      const AMarksSystem: IMarksSystem
    );
  end;

implementation

uses
  StrUtils,
  i_MarksSimple,
  u_BinaryData;

const
  CDescriptionSuffix = '/Description';

{ TInternalDomainInfoProviderByMarksSystem }

constructor TInternalDomainInfoProviderByMarksSystem.Create(
  const AMarksSystem: IMarksSystem);
begin
  inherited Create;
  FMarksSystem := AMarksSystem;
end;

function TInternalDomainInfoProviderByMarksSystem.LoadBinaryByFilePath(
  const AFilePath: string; out AContentType: string): IBinaryData;
var
  VMarkId: string;
  VMark: IMark;
  VDesc: string;
begin
  if Length(AFilePath) <= Length(CDescriptionSuffix) then begin
    Result := nil;
    AContentType := '';
    Exit;
  end;
  VMarkId := LeftStr(AFilePath, Length(AFilePath) - Length(CDescriptionSuffix));
  VMark := FMarksSystem.GetMarkByStringId(VMarkId);
  if VMark = nil then begin
    Result := nil;
    AContentType := '';
    Exit;
  end;
  VDesc := VMark.Desc;
  if VDesc <> '' then begin
    VDesc := '<HTML><BODY>' + VDesc + '</BODY></HTML>';
    Result :=
      TBinaryData.Create(
        Length(VDesc) * SizeOf(VDesc[1]),
        @VDesc[1],
        False
      );
  end;
  AContentType := 'text/html';
end;

end.
