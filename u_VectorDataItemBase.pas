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

unit u_VectorDataItemBase;

interface

uses
  t_Hash,
  i_LonLatRect,
  i_HtmlToHintTextConverter,
  i_VectorDataItemSimple,
  u_BaseInterfacedObject;

type
  TVectorDataItemBase = class(TBaseInterfacedObject, IVectorDataItemSimple)
  private
    FHintConverter: IHtmlToHintTextConverter;
    FHash: THashValue;
    FName: string;
    FDesc: string;
  protected
    function GetHash: THashValue;
    function GetName: string;
    function GetDesc: string;
    function GetLLRect: ILonLatRect; virtual; abstract;
    function GetHintText: string;
    function GetInfoUrl: string;
    function GetInfoCaption: string;
    function GetInfoHTML: string;
  public
    constructor Create(
      const AHash: THashValue;
      const AHintConverter: IHtmlToHintTextConverter;
      const AName: string;
      const ADesc: string
    );
  end;

implementation

{ TVectorDataItemBase }

constructor TVectorDataItemBase.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AName, ADesc: string
);
begin
  inherited Create;
  FHintConverter := AHintConverter;
  FHash := AHash;
  FName := AName;
  FDesc := ADesc;
end;

function TVectorDataItemBase.GetDesc: string;
begin
  Result := FDesc;
end;

function TVectorDataItemBase.GetHash: THashValue;
begin
  Result := FHash;
end;

function TVectorDataItemBase.GetHintText: string;
begin
  Result := FHintConverter.Convert(FName, '');
  if Result = '' then begin
    Result := FHintConverter.Convert(FName, FDesc);
  end;
end;

function TVectorDataItemBase.GetInfoCaption: string;
begin
  Result := FName;
end;

function TVectorDataItemBase.GetInfoHTML: string;
begin
  Result := '';
  if FDesc <> '' then begin
    Result := '<HTML><BODY>';
    Result := Result + FDesc;
    Result := Result + '</BODY></HTML>';
  end;
end;

function TVectorDataItemBase.GetInfoUrl: string;
begin
  Result := '';
end;

function TVectorDataItemBase.GetName: string;
begin
  Result := FName;
end;

end.
