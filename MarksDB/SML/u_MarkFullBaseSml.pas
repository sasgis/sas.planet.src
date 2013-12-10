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

unit u_MarkFullBaseSml;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_Appearance,
  i_LonLatRect,
  i_HtmlToHintTextConverter,
  i_VectorDataItemSimple,
  i_Category,
  u_MarkId;

type
  TMarkFullBaseSml = class(TMarkId, IVectorDataItemSimple, IVectorDataItemWithCategory)
  private
    FHash: THashValue;
    FAppearance: IAppearance;
    FHintConverter: IHtmlToHintTextConverter;
    FDesc: string;
  protected
    function GetHash: THashValue;
    function GetAppearance: IAppearance;
    function GetDesc: string;
    function GetLLRect: ILonLatRect; virtual; abstract;
    function GetHintText: string;
    function GetInfoHTML: string;
    function GetInfoUrl: string;
    function GetInfoCaption: string;
    function GetGoToLonLat: TDoublePoint; virtual; abstract;
    function IsEqual(const AMark: IVectorDataItemSimple): Boolean; virtual;
  public
    constructor Create(
      const AHash: THashValue;
      const AAppearance: IAppearance;
      const AHintConverter: IHtmlToHintTextConverter;
      const AName: string;
      AId: Integer;
      ADbId: Integer;
      const ACategory: ICategory;
      const ADesc: string;
      AVisible: Boolean
    );
  end;

implementation

uses
  SysUtils,
  c_InternalBrowser,
  i_MarkDbSmlInternal;

{ TMarkFullBase }

constructor TMarkFullBaseSml.Create(
  const AHash: THashValue;
  const AAppearance: IAppearance;
  const AHintConverter: IHtmlToHintTextConverter;
  const AName: string;
  AId: Integer;
  ADbId: Integer;
  const ACategory: ICategory;
  const ADesc: string;
  AVisible: Boolean
);
begin
  Assert(Assigned(AAppearance));
  inherited Create(AName, AId, ADbId, ACategory, AVisible);
  FHash := AHash;
  FAppearance := AAppearance;
  FHintConverter := AHintConverter;
  FDesc := ADesc;
end;

function TMarkFullBaseSml.GetAppearance: IAppearance;
begin
  Result := FAppearance;
end;

function TMarkFullBaseSml.GetDesc: string;
begin
  Result := FDesc;
end;

function TMarkFullBaseSml.GetHash: THashValue;
begin
  Result := FHash;
end;

function TMarkFullBaseSml.GetHintText: string;
begin
  Result := FHintConverter.Convert(GetName, FDesc);
end;

function TMarkFullBaseSml.GetInfoCaption: string;
begin
  Result := GetName;
end;

function TMarkFullBaseSml.GetInfoHTML: string;
begin
  Result := '';
  if FDesc <> '' then begin
    Result := '<HTML><BODY>';
    Result := Result + FDesc;
    Result := Result + '</BODY></HTML>';
  end;
end;

function TMarkFullBaseSml.GetInfoUrl: string;
begin
  Result := GetStringID;
  if Result <> '' then begin
    Result := CMarksSystemInternalURL + Result + '/';
  end;
end;

function TMarkFullBaseSml.IsEqual(const AMark: IVectorDataItemSimple): Boolean;
var
  VMarkInternal: IMarkSMLInternal;
begin
  if (AMark.Hash <> 0) and (FHash <> 0) and (AMark.Hash <> FHash) then begin
    Result := False;
    Exit;
  end;
  if not FAppearance.IsEqual(AMark.Appearance) then begin
    Result := False;
    Exit;
  end;
  Result := False;
  if Supports(AMark, IMarkSMLInternal, VMarkInternal) then begin
    if IsEqualInternal(VMarkInternal) then begin
      if FDesc = AMark.Desc then begin
        Result := True;
      end;
    end;
  end;
end;

end.
