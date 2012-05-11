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

unit u_TreeByPathDetalizeProviderList;

interface

uses
  Classes,
  u_TreeChangeableBase,
  i_PathDetalizeProviderList;

type
  TTreeByPathDetalizeProviderList = class(TTreeChangeableBase)
  private
    FProviderList: IPathDetalizeProviderList;
  protected
    function GetSource: IInterface; override;
  public
    constructor Create(const AProviderList: IPathDetalizeProviderList);
  end;


implementation

uses
  ActiveX,
  u_StaticTreeBuilderBase;

type
  TStaticTreeByPathDetalizeProviderListBuilder = class(TStaticTreeBuilderBaseBySlash)
  protected
    procedure ProcessItems(
      const ASource: IInterface;
      AList: TStringList
    ); override;
    function GetNameFromItem(
      const ASource: IInterface;
      const AItem: IInterface
    ): string; override;
  public
    constructor Create;
  end;

{ TStaticTreeByPathDetalizeProviderListBuilder }

constructor TStaticTreeByPathDetalizeProviderListBuilder.Create;
begin
  inherited Create('\', '|');
end;

function TStaticTreeByPathDetalizeProviderListBuilder.GetNameFromItem(
  const ASource: IInterface;
  const AItem: IInterface
): string;
begin
  Result := (AItem as IPathDetalizeProviderListEntity).MenuItemName;
end;

procedure TStaticTreeByPathDetalizeProviderListBuilder.ProcessItems(
  const ASource: IInterface;
  AList: TStringList
);
var
  VList: IPathDetalizeProviderList;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VItem: IPathDetalizeProviderListEntity;
begin
  VList := ASource as IPathDetalizeProviderList;
  VEnum := VList.GetGUIDEnum;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    VItem := VList.Get(VGUID);
    ProcessItem(ASource, VItem, AList);
  end;
end;

{ TTreeByPathDetalizeProviderList }

constructor TTreeByPathDetalizeProviderList.Create(
  const AProviderList: IPathDetalizeProviderList
);
begin
  FProviderList := AProviderList;
  inherited Create(
    TStaticTreeByPathDetalizeProviderListBuilder.Create,
    FProviderList.GetChangeNotifier
  );
end;

function TTreeByPathDetalizeProviderList.GetSource: IInterface;
begin
  Result := FProviderList;
end;

end.
