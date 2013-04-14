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

unit u_MarkFullBase;

interface

uses
  t_GeoTypes,
  i_LonLatRect,
  i_HtmlToHintTextConverter,
  i_VectorDataItemSimple,
  i_Category,
  i_Mark,
  u_BaseInterfacedObject;

type
  TMarkFullBase = class(TBaseInterfacedObject, IMark,
    IVectorDataItemSimple, IVectorDataItemWithCategory)
  private
    FName: string;
    FHintConverter: IHtmlToHintTextConverter;
    FDesc: string;
    FCategory: ICategory;
  protected
    function GetStringID: string;
    function GetName: string;
    function GetMarkType: TGUID; virtual; abstract;
    function GetDesc: string;
    function GetLLRect: ILonLatRect; virtual; abstract;
    function GetHintText: string;
    function GetInfoHTML: string;
    function GetInfoUrl: string;
    function GetInfoCaption: string;
    function GetGoToLonLat: TDoublePoint; virtual; abstract;
    function IsEqual(const AMark: IMark): Boolean; virtual;
    function GetCategory: ICategory;
  public
    constructor Create(
      const AHintConverter: IHtmlToHintTextConverter;
      const AName: string;
      const ACategory: ICategory;
      const ADesc: string
    );
  end;

implementation

{ TMarkFullBase }

constructor TMarkFullBase.Create(
  const AHintConverter: IHtmlToHintTextConverter;
  const AName: string;
  const ACategory: ICategory;
  const ADesc: string
);
begin
  inherited Create;
  FName := AName;
  FCategory := ACategory;
  FHintConverter := AHintConverter;
  FDesc := ADesc;
end;

function TMarkFullBase.GetCategory: ICategory;
begin
  Result := FCategory;
end;

function TMarkFullBase.GetDesc: string;
begin
  Result := FDesc;
end;

function TMarkFullBase.GetHintText: string;
begin
  Result := FHintConverter.Convert(GetName, FDesc);
end;

function TMarkFullBase.GetInfoCaption: string;
begin
  Result := GetName;
end;

function TMarkFullBase.GetInfoHTML: string;
begin
  Result := '';
  if FDesc <> '' then begin
    Result := '<HTML><BODY>';
    Result := Result + FDesc;
    Result := Result + '</BODY></HTML>';
  end;
end;

function TMarkFullBase.GetInfoUrl: string;
begin
  Result := '';
end;

function TMarkFullBase.GetName: string;
begin
  Result := FName;
end;

function TMarkFullBase.GetStringID: string;
begin
  Result := '';
end;

function TMarkFullBase.IsEqual(const AMark: IMark): Boolean;
begin
  Result := True;
  if FName <> AMark.Name then begin
    Result := False;
    Exit;
  end;
  if FDesc <> AMark.Desc then begin
    Result := False;
    Exit;
  end;
  if FCategory <> nil then begin
    if not FCategory.IsSame(AMark.Category) then begin
      Result := False;
      Exit;
    end;
  end else begin
    if AMark.Category <> nil then begin
      Result := False;
      Exit;
    end;
  end;
end;

end.
