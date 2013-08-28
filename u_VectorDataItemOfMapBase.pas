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

unit u_VectorDataItemOfMapBase;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_LonLatRect,
  i_Appearance,
  i_StringProvider,
  i_HtmlToHintTextConverter,
  i_VectorDataItemSimple,
  u_BaseInterfacedObject;

type
  TVectorDataItemOfMapBase = class(TBaseInterfacedObject, IVectorDataItemSimple)
  private
    FUrlPrefix: IStringProvider;
    FIndex: Integer;

    FHintConverter: IHtmlToHintTextConverter;
    FHash: THashValue;
    FName: string;
    FDesc: string;
  protected
    function GetHash: THashValue;
    function GetAppearance: IAppearance;
    function GetName: string;
    function GetDesc: string;
    function GetLLRect: ILonLatRect; virtual; abstract;
    function IsEqual(const AItem: IVectorDataItemSimple): Boolean;
    function GetGoToLonLat: TDoublePoint; virtual; abstract;
    function GetHintText: string;
    function GetInfoUrl: string;
    function GetInfoCaption: string;
    function GetInfoHTML: string;
  public
    constructor Create(
      const AHash: THashValue;
      const AHintConverter: IHtmlToHintTextConverter;
      const AUrlPrefix: IStringProvider;
      const AIndex: Integer;
      const AName: string;
      const ADesc: string
    );
  end;

implementation

uses
  SysUtils;

{ TVectorDataItemBase }

constructor TVectorDataItemOfMapBase.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AUrlPrefix: IStringProvider;
  const AIndex: Integer;
  const AName, ADesc: string
);
begin
  inherited Create;
  FHash := AHash;
  FHintConverter := AHintConverter;
  FUrlPrefix := AUrlPrefix;
  FIndex := AIndex;
  FName := AName;
  FDesc := ADesc;
end;

function TVectorDataItemOfMapBase.GetAppearance: IAppearance;
begin
  Result := nil;
end;

function TVectorDataItemOfMapBase.GetDesc: string;
begin
  Result := FDesc;
end;

function TVectorDataItemOfMapBase.GetHash: THashValue;
begin
  Result := FHash;
end;

function TVectorDataItemOfMapBase.GetHintText: string;
begin
  Result := FHintConverter.Convert(FName, '');
  if Result = '' then begin
    Result := FHintConverter.Convert(FName, FDesc);
  end;
end;

function TVectorDataItemOfMapBase.GetInfoCaption: string;
begin
  Result := FName;
end;

function TVectorDataItemOfMapBase.GetInfoHTML: string;
begin
  Result := '';
  if FDesc <> '' then begin
    Result := '<HTML><BODY>';
    Result := Result + FDesc;
    Result := Result + '</BODY></HTML>';
  end;
end;

function TVectorDataItemOfMapBase.GetInfoUrl: string;
begin
  Result := FUrlPrefix.GetValue + IntToStr(FIndex) + '/';
end;

function TVectorDataItemOfMapBase.GetName: string;
begin
  Result := FName;
end;

function TVectorDataItemOfMapBase.IsEqual(
  const AItem: IVectorDataItemSimple): Boolean;
begin
  Result := True;
  if (AItem.Hash <> 0) and (FHash <> 0) and (AItem.Hash <> FHash) then begin
    Result := False;
    Exit;
  end;
  if Assigned(AItem.Appearance) then begin
    Result := False;
    Exit;
  end;
  if FName <> AItem.Name then begin
    Result := False;
    Exit;
  end;
  if FDesc <> AItem.Desc then begin
    Result := False;
    Exit;
  end;
end;

end.
