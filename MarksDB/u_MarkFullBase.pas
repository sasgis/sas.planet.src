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

unit u_MarkFullBase;

interface

uses
  t_Hash,
  i_HtmlToHintTextConverter,
  i_VectorDataItemSimple,
  i_Category,
  u_BaseInterfacedObject;

type
  TMarkMainInfo = class(TBaseInterfacedObject, IVectorDataItemMainInfo, IVectorDataItemWithCategory)
  private
    FHintConverter: IHtmlToHintTextConverter;
    FHash: THashValue;
    FName: string;
    FDesc: string;
    FCategory: ICategory;
  protected
    function GetHash: THashValue;
    function GetName: string;
    function GetDesc: string;
    function GetHintText: string;
    function GetInfoHTML: string;
    function GetInfoUrl: string;
    function GetInfoCaption: string;
    function IsEqual(const AValue: IVectorDataItemMainInfo): Boolean;
    function GetCategory: ICategory;
  public
    constructor Create(
      const AHash: THashValue;
      const AHintConverter: IHtmlToHintTextConverter;
      const AName: string;
      const ACategory: ICategory;
      const ADesc: string
    );
  end;

implementation

uses
  SysUtils;

{ TMarkMainInfo }

constructor TMarkMainInfo.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AName: string;
  const ACategory: ICategory;
  const ADesc: string
);
begin
  Assert(Assigned(AHintConverter));
  inherited Create;
  FHash := AHash;
  FName := AName;
  FCategory := ACategory;
  FHintConverter := AHintConverter;
  FDesc := ADesc;
end;

function TMarkMainInfo.GetCategory: ICategory;
begin
  Result := FCategory;
end;

function TMarkMainInfo.GetDesc: string;
begin
  Result := FDesc;
end;

function TMarkMainInfo.GetHash: THashValue;
begin
  Result := FHash;
end;

function TMarkMainInfo.GetHintText: string;
begin
  Result := FHintConverter.Convert(GetName, FDesc);
end;

function TMarkMainInfo.GetInfoCaption: string;
begin
  Result := GetName;
end;

function TMarkMainInfo.GetInfoHTML: string;
begin
  Result := '';
  if FDesc <> '' then begin
    Result := '<HTML><BODY>';
    Result := Result + FDesc;
    Result := Result + '</BODY></HTML>';
  end;
end;

function TMarkMainInfo.GetInfoUrl: string;
begin
  Result := '';
end;

function TMarkMainInfo.GetName: string;
begin
  Result := FName;
end;

function TMarkMainInfo.IsEqual(const AValue: IVectorDataItemMainInfo): Boolean;
var
  VVectorDataItemWithCategory: IVectorDataItemWithCategory;
begin
  if not Assigned(AValue) then begin
    Result := False;
    Exit;
  end;
  if AValue = IVectorDataItemMainInfo(Self) then begin
    Result := True;
    Exit;
  end;
  if (AValue.Hash <> 0) and (FHash <> 0) and (AValue.Hash <> FHash) then begin
    Result := False;
    Exit;
  end;
  if FName <> AValue.Name then begin
    Result := False;
    Exit;
  end;
  if FDesc <> AValue.Desc then begin
    Result := False;
    Exit;
  end;
  if not Supports(AValue, IVectorDataItemWithCategory, VVectorDataItemWithCategory) then begin
    Result := False;
    Exit;
  end;
  if FCategory <> nil then begin
    if not FCategory.IsSame(VVectorDataItemWithCategory.Category) then begin
      Result := False;
      Exit;
    end;
  end else begin
    if VVectorDataItemWithCategory.Category <> nil then begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

end.
