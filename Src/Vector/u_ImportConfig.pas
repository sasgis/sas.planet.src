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

unit u_ImportConfig;

interface

uses
  i_Category,
  i_Appearance,
  i_AppearanceOfVectorItem,
  i_ImportConfig,
  u_BaseInterfacedObject;

type
  TImportPointParams = class(TBaseInterfacedObject, IImportPointParams)
  private
    FAppearance: IAppearance;
    FCaptionAppearance: IAppearancePointCaption;
    FIconAppearance: IAppearancePointIcon;
    FIsForceTextColor: Boolean;
    FIsForceTextBgColor: Boolean;
    FIsForceFontSize: Boolean;
    FIsForceMarkerSize: Boolean;
    FIsForcePicName: Boolean;
  private
    function GetAppearance: IAppearance;
    function GetCaptionAppearance: IAppearancePointCaption;
    function GetIconAppearance: IAppearancePointIcon;
    function GetIsForceTextColor: Boolean;
    function GetIsForceTextBgColor: Boolean;
    function GetIsForceFontSize: Boolean;
    function GetIsForceMarkerSize: Boolean;
    function GetIsForcePicName: Boolean;
  public
    constructor Create(
      const AAppearance: IAppearance;
      const AIsForceTextColor: Boolean;
      const AIsForceTextBgColor: Boolean;
      const AIsForceFontSize: Boolean;
      const AIsForceMarkerSize: Boolean;
      const AIsForcePicName: Boolean
    );
  end;

  TImportLineParams = class(TBaseInterfacedObject, IImportLineParams)
  private
    FAppearance: IAppearance;
    FLineAppearance: IAppearanceLine;
    FIsForceLineColor: Boolean;
    FIsForceLineWidth: Boolean;
  private
    function GetAppearance: IAppearance;
    function GetLineAppearance: IAppearanceLine;
    function GetIsForceLineColor: Boolean;
    function GetIsForceLineWidth: Boolean;
  public
    constructor Create(
      const AAppearance: IAppearance;
      const AIsForceLineColor: Boolean;
      const AIsForceLineWidth: Boolean
    );
  end;

  TImportPolyParams = class(TBaseInterfacedObject, IImportPolyParams)
  private
    FAppearance: IAppearance;
    FBorderAppearance: IAppearancePolygonBorder;
    FFillAppearance: IAppearancePolygonFill;
    FIsForceLineColor: Boolean;
    FIsForceLineWidth: Boolean;
    FIsForceFillColor: Boolean;
  private
    function GetAppearance: IAppearance;
    function GetBorderAppearance: IAppearancePolygonBorder;
    function GetFillAppearance: IAppearancePolygonFill;
    function GetIsForceLineColor: Boolean;
    function GetIsForceLineWidth: Boolean;
    function GetIsForceFillColor: Boolean;
  public
    constructor Create(
      const AAppearance: IAppearance;
      const AIsForceLineColor: Boolean;
      const AIsForceLineWidth: Boolean;
      const AIsForceFillColor: Boolean
    );
  end;

  TImportCategoryParams = class(TBaseInterfacedObject, IImportCategoryParams)
  private
    FIsAddAllInRootCategory: Boolean;
    FIsIgnoreMarkIfSubCategotyNotEixts: Boolean;
    FIsCreateSubCategory: Boolean;
    FIsIgnoreMarkIfExistsWithSameNameInCategory: Boolean;
  private
    function GetIsAddAllInRootCategory: Boolean;
    function GetIsIgnoreMarkIfSubCategotyNotEixts: Boolean;
    function GetIsCreateSubCategory: Boolean;
    function GetIsIgnoreMarkIfExistsWithSameNameInCategory: Boolean;
  public
    constructor Create(
      const AIsAddAllInRootCategory: Boolean;
      const AIsIgnoreMarkIfSubCategotyNotEixts: Boolean;
      const AIsCreateSubCategory: Boolean;
      const AIsIgnoreMarkIfExistsWithSameNameInCategory: Boolean
    );
  end;

  TImportConfig = class(TBaseInterfacedObject, IImportConfig)
  private
    FRootCategory: ICategory;
    FCategoryParams: IImportCategoryParams;
    FPointParams: IImportPointParams;
    FLineParams: IImportLineParams;
    FPolyParams: IImportPolyParams;
  protected
    function GetRootCategory: ICategory;
    function GetCategoryParams: IImportCategoryParams;
    function GetPointParams: IImportPointParams;
    function GetLineParams: IImportLineParams;
    function GetPolyParams: IImportPolyParams;
  public
    constructor Create(
      const ARootCategory: ICategory;
      const ACategoryParams: IImportCategoryParams;
      const APointParams: IImportPointParams;
      const ALineParams: IImportLineParams;
      const APolyParams: IImportPolyParams
    );
  end;

implementation

uses
  SysUtils;

{ TImportPointParams }

constructor TImportPointParams.Create(
  const AAppearance: IAppearance;
  const AIsForceTextColor: Boolean;
  const AIsForceTextBgColor: Boolean;
  const AIsForceFontSize: Boolean;
  const AIsForceMarkerSize: Boolean;
  const AIsForcePicName: Boolean
);
begin
  Assert(Supports(AAppearance, IAppearancePointCaption));
  Assert(Supports(AAppearance, IAppearancePointIcon));
  inherited Create;
  FAppearance := AAppearance;
  FCaptionAppearance := AAppearance as IAppearancePointCaption;
  FIconAppearance := AAppearance as IAppearancePointIcon;
  FIsForceTextColor := AIsForceTextColor;
  FIsForceTextBgColor := AIsForceTextBgColor;
  FIsForceFontSize := AIsForceFontSize;
  FIsForceMarkerSize := AIsForceMarkerSize;
  FIsForcePicName := AIsForcePicName;
end;

function TImportPointParams.GetAppearance: IAppearance;
begin
  Result :=FAppearance;
end;

function TImportPointParams.GetCaptionAppearance: IAppearancePointCaption;
begin
  Result := FCaptionAppearance;
end;

function TImportPointParams.GetIconAppearance: IAppearancePointIcon;
begin
  Result := FIconAppearance;
end;

function TImportPointParams.GetIsForceFontSize: Boolean;
begin
  Result := FIsForceFontSize;
end;

function TImportPointParams.GetIsForceMarkerSize: Boolean;
begin
  Result := FIsForceMarkerSize;
end;

function TImportPointParams.GetIsForcePicName: Boolean;
begin
  Result := FIsForcePicName;
end;

function TImportPointParams.GetIsForceTextBgColor: Boolean;
begin
  Result := FIsForceTextBgColor;
end;

function TImportPointParams.GetIsForceTextColor: Boolean;
begin
  Result := FIsForceTextColor;
end;

{ TImportLineParams }

constructor TImportLineParams.Create(
  const AAppearance: IAppearance;
  const AIsForceLineColor: Boolean;
  const AIsForceLineWidth: Boolean
);
begin
  Assert(Supports(AAppearance, IAppearanceLine));
  inherited Create;
  FAppearance := AAppearance;
  FLineAppearance := AAppearance as IAppearanceLine;
  FIsForceLineColor := AIsForceLineWidth;
  FIsForceLineWidth := AIsForceLineWidth;
end;

function TImportLineParams.GetAppearance: IAppearance;
begin
  Result :=FAppearance;
end;

function TImportLineParams.GetIsForceLineColor: Boolean;
begin
  Result := FIsForceLineColor;
end;

function TImportLineParams.GetIsForceLineWidth: Boolean;
begin
  Result := FIsForceLineWidth;
end;

function TImportLineParams.GetLineAppearance: IAppearanceLine;
begin
  Result := FLineAppearance;
end;

{ TImportPolyParams }

constructor TImportPolyParams.Create(
  const AAppearance: IAppearance;
  const AIsForceLineColor: Boolean;
  const AIsForceLineWidth: Boolean;
  const AIsForceFillColor: Boolean
);
begin
  Assert(Supports(AAppearance, IAppearancePolygonBorder));
  Assert(Supports(AAppearance, IAppearancePolygonFill));
  inherited Create;
  FAppearance := AAppearance;
  FBorderAppearance := AAppearance as IAppearancePolygonBorder;
  FFillAppearance := AAppearance as IAppearancePolygonFill;
  FIsForceLineColor := AIsForceLineWidth;
  FIsForceLineWidth := AIsForceLineWidth;
  FIsForceFillColor := AIsForceFillColor;
end;

function TImportPolyParams.GetAppearance: IAppearance;
begin
  Result :=FAppearance;
end;

function TImportPolyParams.GetBorderAppearance: IAppearancePolygonBorder;
begin
  Result := FBorderAppearance;
end;

function TImportPolyParams.GetFillAppearance: IAppearancePolygonFill;
begin
  Result := FFillAppearance;
end;

function TImportPolyParams.GetIsForceFillColor: Boolean;
begin
  Result := FIsForceFillColor;
end;

function TImportPolyParams.GetIsForceLineColor: Boolean;
begin
  Result := FIsForceLineColor;
end;

function TImportPolyParams.GetIsForceLineWidth: Boolean;
begin
  Result := FIsForceLineWidth;
end;

{ TImportCategoryParams }

constructor TImportCategoryParams.Create(
  const AIsAddAllInRootCategory: Boolean;
  const AIsIgnoreMarkIfSubCategotyNotEixts: Boolean;
  const AIsCreateSubCategory: Boolean;
  const AIsIgnoreMarkIfExistsWithSameNameInCategory: Boolean
);
begin
  inherited Create;
  FIsAddAllInRootCategory := AIsAddAllInRootCategory;
  FIsIgnoreMarkIfSubCategotyNotEixts := AIsIgnoreMarkIfSubCategotyNotEixts;
  FIsCreateSubCategory := AIsCreateSubCategory;
  FIsIgnoreMarkIfExistsWithSameNameInCategory := AIsIgnoreMarkIfExistsWithSameNameInCategory;
end;

function TImportCategoryParams.GetIsAddAllInRootCategory: Boolean;
begin
  Result := FIsAddAllInRootCategory;
end;

function TImportCategoryParams.GetIsCreateSubCategory: Boolean;
begin
  Result := FIsCreateSubCategory;
end;

function TImportCategoryParams.GetIsIgnoreMarkIfExistsWithSameNameInCategory: Boolean;
begin
  Result := FIsIgnoreMarkIfExistsWithSameNameInCategory;
end;

function TImportCategoryParams.GetIsIgnoreMarkIfSubCategotyNotEixts: Boolean;
begin
  Result := FIsIgnoreMarkIfSubCategotyNotEixts;
end;

{ TImportConfig }

constructor TImportConfig.Create(
  const ARootCategory: ICategory;
  const ACategoryParams: IImportCategoryParams;
  const APointParams: IImportPointParams;
  const ALineParams: IImportLineParams;
  const APolyParams: IImportPolyParams
);
begin
  inherited Create;
  FRootCategory := ARootCategory;
  FCategoryParams := ACategoryParams;
  FPointParams := APointParams;
  FLineParams := ALineParams;
  FPolyParams := APolyParams;
end;

function TImportConfig.GetCategoryParams: IImportCategoryParams;
begin
  Result := FCategoryParams;
end;

function TImportConfig.GetLineParams: IImportLineParams;
begin
  Result := FLineParams;
end;

function TImportConfig.GetPointParams: IImportPointParams;
begin
  Result := FPointParams;
end;

function TImportConfig.GetPolyParams: IImportPolyParams;
begin
  Result := FPolyParams;
end;

function TImportConfig.GetRootCategory: ICategory;
begin
  Result := FRootCategory;
end;

end.
