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
  i_Appearance,
  i_HtmlToHintTextConverter,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  u_BaseInterfacedObject;

type
  TVectorDataItemMainInfo = class(TBaseInterfacedObject, IVectorDataItemMainInfo)
  private
    FHash: THashValue;
    FHintConverter: IHtmlToHintTextConverter;
    FName: string;
    FDesc: string;
  private
    function GetHash: THashValue;
    function GetName: string;
    function GetDesc: string;
    function IsEqual(const AItem: IVectorDataItemMainInfo): Boolean;
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

  TVectorDataItemBase = class(TBaseInterfacedObject, IVectorDataItemSimple)
  private
    FHash: THashValue;
    FMainInfo: IVectorDataItemMainInfo;
    FAppearance: IAppearance;
  protected
    function GetHash: THashValue;
    function GetMainInfo: IVectorDataItemMainInfo;
    function GetName: string;
    function GetDesc: string;
    function GetGeometry: IGeometryLonLat; virtual; abstract;
    function GetAppearance: IAppearance;
    function IsEqual(const AItem: IVectorDataItemSimple): Boolean;
    function GetHintText: string;
    function GetInfoUrl: string;
    function GetInfoCaption: string;
    function GetInfoHTML: string;
  public
    constructor Create(
      const AHash: THashValue;
      const AAppearance: IAppearance;
      const AMainInfo: IVectorDataItemMainInfo
    );
  end;

implementation

{ TVectorDataItemBase }

constructor TVectorDataItemBase.Create(
  const AHash: THashValue;
  const AAppearance: IAppearance;
  const AMainInfo: IVectorDataItemMainInfo
);
begin
  Assert(Assigned(AMainInfo));
  inherited Create;
  FAppearance := AAppearance;
  FHash := AHash;
  FMainInfo := AMainInfo;
end;

function TVectorDataItemBase.GetAppearance: IAppearance;
begin
  Result := FAppearance;
end;

function TVectorDataItemBase.GetDesc: string;
begin
  Result := FMainInfo.Desc;
end;

function TVectorDataItemBase.GetHash: THashValue;
begin
  Result := FHash;
end;

function TVectorDataItemBase.GetHintText: string;
begin
  Result := FMainInfo.GetHintText;
end;

function TVectorDataItemBase.GetInfoCaption: string;
begin
  Result := FMainInfo.GetInfoCaption;
end;

function TVectorDataItemBase.GetInfoHTML: string;
begin
  Result := FMainInfo.GetInfoHTML;
end;

function TVectorDataItemBase.GetInfoUrl: string;
begin
  Result := FMainInfo.GetInfoUrl;
end;

function TVectorDataItemBase.GetMainInfo: IVectorDataItemMainInfo;
begin
  Result := FMainInfo;
end;

function TVectorDataItemBase.GetName: string;
begin
  Result := FMainInfo.Name;
end;

function TVectorDataItemBase.IsEqual(
  const AItem: IVectorDataItemSimple
): Boolean;
begin
  if not Assigned(AItem) then begin
    Result := False;
    Exit;
  end;

  if AItem = IVectorDataItemSimple(Self) then begin
    Result := True;
    Exit;
  end;

  if (AItem.Hash <> 0) and (FHash <> 0) and (AItem.Hash <> FHash) then begin
    Result := False;
    Exit;
  end;

  if Assigned(FAppearance) then begin
    if not FAppearance.IsEqual(AItem.Appearance) then begin
      Result := False;
      Exit;
    end;
  end else begin
    if Assigned(AItem.Appearance) then begin
      Result := False;
      Exit;
    end;
  end;
  if not FMainInfo.IsEqual(AItem.MainInfo) then begin
    Result := False;
    Exit;
  end;
  Result := GetGeometry.IsSameGeometry(AItem.Geometry);
end;

{ TVectorDataItemMainInfo }

constructor TVectorDataItemMainInfo.Create(
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

function TVectorDataItemMainInfo.GetDesc: string;
begin
  Result := FDesc;
end;

function TVectorDataItemMainInfo.GetHash: THashValue;
begin
  Result := FHash;
end;

function TVectorDataItemMainInfo.GetHintText: string;
begin
  Result := FHintConverter.Convert(FName, '');
  if Result = '' then begin
    Result := FHintConverter.Convert(FName, FDesc);
  end;
end;

function TVectorDataItemMainInfo.GetInfoCaption: string;
begin
  Result := FName;
end;

function TVectorDataItemMainInfo.GetInfoHTML: string;
begin
  Result := '';
  if FDesc <> '' then begin
    Result := '<HTML><BODY>';
    Result := Result + FDesc;
    Result := Result + '</BODY></HTML>';
  end;
end;

function TVectorDataItemMainInfo.GetInfoUrl: string;
begin
  Result := '';
end;

function TVectorDataItemMainInfo.GetName: string;
begin
  Result := FName;
end;

function TVectorDataItemMainInfo.IsEqual(
  const AItem: IVectorDataItemMainInfo
): Boolean;
begin
  if not Assigned(AItem) then begin
    Result := False;
    Exit;
  end;
  if AItem = IVectorDataItemMainInfo(Self) then begin
    Result := True;
    Exit;
  end;
  if (AItem.Hash <> 0) and (FHash <> 0) and (AItem.Hash <> FHash) then begin
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
  Result := True;
end;

end.
