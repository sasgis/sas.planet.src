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

unit u_InternalDomainInfoProviderByLastSearchResults;

interface

uses
  Classes,
  i_BinaryData,
  i_VectorDataItemSimple,
  i_LastSearchResultConfig,
  i_TextByVectorItem,
  i_InternalDomainInfoProvider,
  u_BaseInterfacedObject;

type
  TInternalDomainInfoProviderByLastSearchResults = class(TBaseInterfacedObject, IInternalDomainInfoProvider)
  private
    FLastSearchResults: ILastSearchResultConfig;
    FTextProviders: TStringList;
    FDefaultProvider: ITextByVectorItem;
    function GetItemByIndex(const AIndex: Integer): IVectorDataItemSimple;
    function BuildBinaryDataByText(const AText: string): IBinaryData;
    function ParseFileName(
      const AFilePath: string;
      out ASearchResultIndex: Integer;
      out ASuffix: string
    ): Boolean;
  private
    function LoadBinaryByFilePath(
      const AFilePath: string;
      out AContentType: string
    ): IBinaryData;
  public
    constructor Create(
      const ALastSearchResults: ILastSearchResultConfig;
      const ADefaultProvider: ITextByVectorItem;
      ATextProviders: TStringList
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  i_GeoCoder,
  u_BinaryData;

{ TInternalDomainInfoProviderByMarksSystem }

constructor TInternalDomainInfoProviderByLastSearchResults.Create(
  const ALastSearchResults: ILastSearchResultConfig;
  const ADefaultProvider: ITextByVectorItem;
  ATextProviders: TStringList
);
begin
  inherited Create;
  FLastSearchResults := ALastSearchResults;
  FDefaultProvider := ADefaultProvider;
  FTextProviders := ATextProviders;
end;

destructor TInternalDomainInfoProviderByLastSearchResults.Destroy;
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

function TInternalDomainInfoProviderByLastSearchResults.GetItemByIndex(
  const AIndex: Integer
): IVectorDataItemSimple;
var
  VLastResult: IGeoCodeResult;
begin
  Result := nil;
  VLastResult := FLastSearchResults.GeoCodeResult;
  if (VLastResult <> nil) and (VLastResult.Count > 0) and (AIndex < VLastResult.Count) then begin
    Result := VLastResult.Items[AIndex];
  end;
end;

function TInternalDomainInfoProviderByLastSearchResults.BuildBinaryDataByText(
  const AText: string): IBinaryData;
begin
  Result := nil;
  if AText <> '' then begin
    Result := TBinaryData.CreateByString(AText);
  end;
end;

function TInternalDomainInfoProviderByLastSearchResults.LoadBinaryByFilePath(
  const AFilePath: string; out AContentType: string): IBinaryData;
var
  VSearchResultIndex: Integer;
  VSearchResult: IVectorDataItemSimple;
  VProvider: ITextByVectorItem;
  VSuffix: string;
  VProviderIndex: Integer;
  VText: string;
begin
  Result := nil;
  AContentType := 'text/html';
  if ParseFileName(AFilePath, VSearchResultIndex, VSuffix) then begin
    VSearchResult := GetItemByIndex(VSearchResultIndex);
    if VSearchResult <> nil then begin
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
      VText := VProvider.GetText(VSearchResult);
      Result := BuildBinaryDataByText(VText);
      AContentType := 'text/html';
    end;
  end;
end;

function TInternalDomainInfoProviderByLastSearchResults.ParseFileName(
  const AFilePath: string;
  out ASearchResultIndex: Integer;
  out ASuffix: string
): Boolean;
var
  VFileNameLen: Integer;
  VSlashPos: Integer;
  i: Integer;
  VIndexStr: string;
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
    VIndexStr := LeftStr(AFilePath, VSlashPos - 1);
    ASuffix := RightStr(AFilePath, VFileNameLen - VSlashPos);
  end else begin
    VIndexStr := AFilePath;
    ASuffix := '';
  end;
  if TryStrToInt(VIndexStr, ASearchResultIndex) then begin
    Result := True;
  end else begin
    ASearchResultIndex := 0;
    Result := False;
  end;
end;

end.
