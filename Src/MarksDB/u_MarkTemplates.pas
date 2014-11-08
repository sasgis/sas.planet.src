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

unit u_MarkTemplates;

interface

uses
  i_Category,
  i_Appearance,
  i_AppearanceOfVectorItem,
  i_MarkTemplate,
  i_MarkNameGenerator,
  u_BaseInterfacedObject;

type
  TMarkTemplateBase = class(TBaseInterfacedObject, IMarkTemplate)
  private
    FNameGenerator: IMarkNameGenerator;
    FAppearance: IAppearance;
    FCategory: ICategory;
    function IsSameInternal(const ATemplate: IMarkTemplate): Boolean;
  protected
    function GetNewName: string;
    function GetAppearance: IAppearance;
    function GetCategory: ICategory;
  public
    constructor Create(
      const ANameGenerator: IMarkNameGenerator;
      const AAppearance: IAppearance;
      const ACategory: ICategory
    );
  end;

  TMarkTemplatePoint = class(TMarkTemplateBase, IMarkTemplatePoint)
  private
    FCaptionAppearance: IAppearancePointCaption;
    FIconAppearance: IAppearancePointIcon;
  protected
    function GetCaptionAppearance: IAppearancePointCaption;
    function GetIconAppearance: IAppearancePointIcon;

    function IsSame(const ATemplate: IMarkTemplatePoint): Boolean;
  public
    constructor Create(
      const ANameGenerator: IMarkNameGenerator;
      const ACategory: ICategory;
      const AAppearance: IAppearance
    );
  end;

  TMarkTemplateLine = class(TMarkTemplateBase, IMarkTemplateLine)
  private
    FLineAppearance: IAppearanceLine;
  protected
    function GetLineAppearance: IAppearanceLine;
    function IsSame(const ATemplate: IMarkTemplateLine): Boolean;
  public
    constructor Create(
      const ANameGenerator: IMarkNameGenerator;
      const ACategory: ICategory;
      const AAppearance: IAppearance
    );
  end;

  TMarkTemplatePoly = class(TMarkTemplateBase, IMarkTemplatePoly)
  private
    FBorderAppearance: IAppearancePolygonBorder;
    FFillAppearance: IAppearancePolygonFill;
  protected
    function GetBorderAppearance: IAppearancePolygonBorder;
    function GetFillAppearance: IAppearancePolygonFill;

    function IsSame(const ATemplate: IMarkTemplatePoly): Boolean;
  public
    constructor Create(
      const ANameGenerator: IMarkNameGenerator;
      const ACategory: ICategory;
      const AAppearance: IAppearance
    );
  end;

implementation

uses
  SysUtils;

{ FMarkTemplateBase }

constructor TMarkTemplateBase.Create(
  const ANameGenerator: IMarkNameGenerator;
  const AAppearance: IAppearance;
  const ACategory: ICategory
);
begin
  Assert(ANameGenerator <> nil);
  Assert(ACategory <> nil);
  Assert(Assigned(AAppearance));
  inherited Create;
  FNameGenerator := ANameGenerator;
  FAppearance := AAppearance;
  FCategory := ACategory;
end;

function TMarkTemplateBase.GetAppearance: IAppearance;
begin
  Result := FAppearance;
end;

function TMarkTemplateBase.GetCategory: ICategory;
begin
  Result := FCategory;
end;

function TMarkTemplateBase.GetNewName: string;
begin
  Result := FNameGenerator.GetNewName;
end;

function TMarkTemplateBase.IsSameInternal(
  const ATemplate: IMarkTemplate
): Boolean;
begin
  Result := False;
  if not FAppearance.IsEqual(ATemplate.Appearance) then begin
    Result := False;
    Exit;
  end;
  if ATemplate <> nil then begin
    if FCategory = nil then begin
      Result := ATemplate.Category = nil;
    end else begin
      Result := FCategory.IsEqual(ATemplate.Category);
    end;
  end;
end;

{ TMarkTemplatePoint }

constructor TMarkTemplatePoint.Create(
  const ANameGenerator: IMarkNameGenerator;
  const ACategory: ICategory;
  const AAppearance: IAppearance
);
begin
  Assert(Supports(AAppearance, IAppearancePointCaption));
  Assert(Supports(AAppearance, IAppearancePointIcon));
  inherited Create(ANameGenerator, AAppearance, ACategory);
  Supports(AAppearance, IAppearancePointCaption, FCaptionAppearance);
  Supports(AAppearance, IAppearancePointIcon, FIconAppearance);
end;

function TMarkTemplatePoint.GetCaptionAppearance: IAppearancePointCaption;
begin
  Result := FCaptionAppearance;
end;

function TMarkTemplatePoint.GetIconAppearance: IAppearancePointIcon;
begin
  Result := FIconAppearance;
end;

function TMarkTemplatePoint.IsSame(const ATemplate: IMarkTemplatePoint): Boolean;
begin
  Result := IsSameInternal(ATemplate);
end;

{ TMarkTemplateLine }

constructor TMarkTemplateLine.Create(
  const ANameGenerator: IMarkNameGenerator;
  const ACategory: ICategory;
  const AAppearance: IAppearance
);
begin
  Assert(Supports(AAppearance, IAppearanceLine));
  inherited Create(ANameGenerator, AAppearance, ACategory);
  Supports(AAppearance, IAppearanceLine, FLineAppearance);
end;

function TMarkTemplateLine.GetLineAppearance: IAppearanceLine;
begin
  Result := FLineAppearance;
end;

function TMarkTemplateLine.IsSame(const ATemplate: IMarkTemplateLine): Boolean;
begin
  Result := IsSameInternal(ATemplate);
end;

{ TMarkTemplatePoly }

constructor TMarkTemplatePoly.Create(
  const ANameGenerator: IMarkNameGenerator;
  const ACategory: ICategory;
  const AAppearance: IAppearance
);
begin
  Assert(Supports(AAppearance, IAppearancePolygonBorder));
  Assert(Supports(AAppearance, IAppearancePolygonFill));
  inherited Create(ANameGenerator, AAppearance, ACategory);
  Supports(AAppearance, IAppearancePolygonBorder, FBorderAppearance);
  Supports(AAppearance, IAppearancePolygonFill, FFillAppearance);
end;

function TMarkTemplatePoly.GetBorderAppearance: IAppearancePolygonBorder;
begin
  Result := FBorderAppearance;
end;

function TMarkTemplatePoly.GetFillAppearance: IAppearancePolygonFill;
begin
  Result := FFillAppearance;
end;

function TMarkTemplatePoly.IsSame(const ATemplate: IMarkTemplatePoly): Boolean;
begin
  Result := IsSameInternal(ATemplate);
end;

end.
