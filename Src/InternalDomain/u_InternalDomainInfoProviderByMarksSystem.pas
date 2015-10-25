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

unit u_InternalDomainInfoProviderByMarksSystem;

interface

uses
  Classes,
  i_BinaryData,
  i_MarkSystem,
  i_TextByVectorItem,
  i_InternalDomainInfoProvider,
  u_BaseInterfacedObject;

type
  TInternalDomainInfoProviderByMarksSystem = class(TBaseInterfacedObject, IInternalDomainInfoProvider)
  private
    FMarksSystem: IMarkSystem;
    FTextProviders: TStringList;
    FDefaultProvider: ITextByVectorItem;
    function BuildBinaryDataByText(const AText: string): IBinaryData;
    function ParseFileName(
      const AFilePath: string;
      out AMarkId: string;
      out ASuffix: string
    ): Boolean;
  private
    function LoadBinaryByFilePath(
      const AFilePath: string;
      out AContentType: AnsiString
    ): IBinaryData;
  public
    constructor Create(
      const AMarksSystem: IMarkSystem;
      const ADefaultProvider: ITextByVectorItem;
      ATextProviders: TStringList
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  i_VectorDataItemSimple,
  u_BinaryData;

{ TInternalDomainInfoProviderByMarksSystem }

constructor TInternalDomainInfoProviderByMarksSystem.Create(
  const AMarksSystem: IMarkSystem;
  const ADefaultProvider: ITextByVectorItem;
  ATextProviders: TStringList
);
begin
  inherited Create;
  FMarksSystem := AMarksSystem;
  FDefaultProvider := ADefaultProvider;
  FTextProviders := ATextProviders;
end;

destructor TInternalDomainInfoProviderByMarksSystem.Destroy;
var
  i: Integer;
  VItem: Pointer;
begin
  if Assigned(FTextProviders) then begin
    for i := 0 to FTextProviders.Count - 1 do begin
      VItem := FTextProviders.Objects[i];
      if VItem <> nil then begin
        IInterface(VItem)._Release;
      end;
    end;
    FreeAndNil(FTextProviders);
  end;
  inherited;
end;

function TInternalDomainInfoProviderByMarksSystem.BuildBinaryDataByText(
  const AText: string): IBinaryData;
begin
  Result := nil;
  if AText <> '' then begin
    Result := TBinaryData.CreateByString(AText);
  end;
end;

function TInternalDomainInfoProviderByMarksSystem.LoadBinaryByFilePath(
  const AFilePath: string;
  out AContentType: AnsiString
): IBinaryData;
var
  VMarkId: string;
  VMark: IVectorDataItem;
  VProvider: ITextByVectorItem;
  VSuffix: string;
  VProviderIndex: Integer;
  VText: string;
begin
  Result := nil;
  AContentType := 'text/html';
  if ParseFileName(AFilePath, VMarkId, VSuffix) then begin
    VMark := FMarksSystem.GetMarkByStringId(VMarkId);
    if VMark <> nil then begin
      VProvider := nil;
      if VSuffix <> '' then begin
        if FTextProviders <> nil then begin
          if FTextProviders.Find(VSuffix, VProviderIndex) then begin
            VProvider := ITextByVectorItem(Pointer(FTextProviders.Objects[VProviderIndex]));
          end;
        end;
      end;
      if VProvider = nil then begin
        VProvider := FDefaultProvider;
      end;
      VText := VProvider.GetText(VMark);
      Result := BuildBinaryDataByText(VText);
      AContentType := 'text/html';
    end;
  end;
end;

function TInternalDomainInfoProviderByMarksSystem.ParseFileName(
  const AFilePath: string;
  out AMarkId, ASuffix: string
): Boolean;
var
  VFileNameLen: Integer;
  VSlashPos: Integer;
  i: Integer;
begin
  VFileNameLen := Length(AFilePath);
  if VFileNameLen = 0 then begin
    Result := False;
    Exit;
  end;

  VSlashPos := 0;
  for i := VFileNameLen downto 1 do begin
    if AFilePath[i] = '/' then begin
      VSlashPos := i;
      Break;
    end;
  end;


  if VSlashPos > 0 then begin
    AMarkId := LeftStr(AFilePath, VSlashPos - 1);
    ASuffix := RightStr(AFilePath, VFileNameLen - VSlashPos);
  end else begin
    AMarkId := AFilePath;
    ASuffix := '';
  end;
  Result := True;
end;

end.
