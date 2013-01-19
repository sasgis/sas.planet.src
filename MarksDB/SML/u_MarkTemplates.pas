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

unit u_MarkTemplates;

interface

uses
  GR32,
  i_MarkTemplate,
  i_MarkCategory,
  i_MarkPicture,
  i_MarksDbSmlInternal,
  i_MarkCategoryDBSmlInternal,
  i_MarkNameGenerator,
  u_BaseInterfacedObject;

type
  TMarkTemplateBase = class(TBaseInterfacedObject, IMarkTemplate, IMarkTemplateSMLInternal)
  private
    FCategoryDb: IMarkCategoryDBSmlInternal;
    FNameGenerator: IMarkNameGenerator;
    FCategoryId: Integer;
    function IsSameInternal(const ATemplate: IMarkTemplate): Boolean;
  protected
    function GetNewName: string;
    function GetCategory: ICategory;
    function GetCategoryId: Integer;
  public
    constructor Create(
      const ACategoryDb: IMarkCategoryDBSmlInternal;
      const ANameGenerator: IMarkNameGenerator;
      ACategoryId: Integer
    );
  end;

  TMarkTemplatePoint = class(TMarkTemplateBase, IMarkTemplatePoint)
  private
    FTextColor: TColor32;
    FTextBgColor: TColor32;
    FFontSize: Integer;
    FMarkerSize: Integer;
    FPic: IMarkPicture;
  protected
    function GetTextColor: TColor32;
    function GetTextBgColor: TColor32;
    function GetFontSize: Integer;
    function GetMarkerSize: Integer;
    function GetPic: IMarkPicture;
    function IsSame(const ATemplate: IMarkTemplatePoint): Boolean;
  public
    constructor Create(
      const ACategoryDb: IMarkCategoryDBSmlInternal;
      const ANameGenerator: IMarkNameGenerator;
      ACategoryId: Integer;
      ATextColor: TColor32;
      ATextBgColor: TColor32;
      AFontSize: Integer;
      AMarkerSize: Integer;
      const APic: IMarkPicture
    );
  end;

  TMarkTemplateLine = class(TMarkTemplateBase, IMarkTemplateLine)
  private
    FLineColor: TColor32;
    FLineWidth: Integer;
  protected
    function GetLineColor: TColor32;
    function GetLineWidth: Integer;
    function IsSame(const ATemplate: IMarkTemplateLine): Boolean;
  public
    constructor Create(
      const ACategoryDb: IMarkCategoryDBSmlInternal;
      const ANameGenerator: IMarkNameGenerator;
      ACategoryId: Integer;
      ALineColor: TColor32;
      ALineWidth: Integer
    );
  end;

  TMarkTemplatePoly = class(TMarkTemplateBase, IMarkTemplatePoly)
  private
    FBorderColor: TColor32;
    FFillColor: TColor32;
    FLineWidth: Integer;
  protected
    function GetBorderColor: TColor32;
    function GetFillColor: TColor32;
    function GetLineWidth: Integer;
    function IsSame(const ATemplate: IMarkTemplatePoly): Boolean;
  public
    constructor Create(
      const ACategoryDb: IMarkCategoryDBSmlInternal;
      const ANameGenerator: IMarkNameGenerator;
      ACategoryId: Integer;
      ABorderColor: TColor32;
      AFillColor: TColor32;
      ALineWidth: Integer
    );
  end;

implementation

uses
  SysUtils;

{ FMarkTemplateBase }

constructor TMarkTemplateBase.Create(
  const ACategoryDb: IMarkCategoryDBSmlInternal;
  const ANameGenerator: IMarkNameGenerator;
  ACategoryId: Integer
);
begin
  inherited Create;
  FNameGenerator := ANameGenerator;
  FCategoryDb := ACategoryDb;
  FCategoryId := ACategoryId;
end;

function TMarkTemplateBase.GetCategory: ICategory;
begin
  Result := FCategoryDb.GetCategoryByID(FCategoryId);
end;

function TMarkTemplateBase.GetCategoryId: Integer;
begin
  Result := FCategoryId;
end;

function TMarkTemplateBase.GetNewName: string;
begin
  Result := FNameGenerator.GetNewName;
end;

function TMarkTemplateBase.IsSameInternal(
  const ATemplate: IMarkTemplate
): Boolean;
var
  VTemplateInternal: IMarkTemplateSMLInternal;
begin
  Result := False;
  if ATemplate <> nil then begin
    if Supports(ATemplate, IMarkTemplateSMLInternal, VTemplateInternal) then begin
      Result := VTemplateInternal.CategoryId = FCategoryId;
    end;
  end;
end;

{ TMarkTemplatePoint }

constructor TMarkTemplatePoint.Create(
  const ACategoryDb: IMarkCategoryDBSmlInternal;
  const ANameGenerator: IMarkNameGenerator;
  ACategoryId: Integer;
  ATextColor, ATextBgColor: TColor32;
  AFontSize, AMarkerSize: Integer;
  const APic: IMarkPicture
);
begin
  inherited Create(ACategoryDb, ANameGenerator, ACategoryId);
  FTextColor := ATextColor;
  FTextBgColor := ATextBgColor;
  FFontSize := AFontSize;
  FMarkerSize := AMarkerSize;
  FPic := APic;
end;

function TMarkTemplatePoint.GetTextColor: TColor32;
begin
  Result := FTextColor;
end;

function TMarkTemplatePoint.GetTextBgColor: TColor32;
begin
  Result := FTextBgColor;
end;

function TMarkTemplatePoint.GetPic: IMarkPicture;
begin
  Result := FPic;
end;

function TMarkTemplatePoint.GetFontSize: Integer;
begin
  Result := FFontSize;
end;

function TMarkTemplatePoint.GetMarkerSize: Integer;
begin
  Result := FMarkerSize;
end;

function TMarkTemplatePoint.IsSame(const ATemplate: IMarkTemplatePoint): Boolean;
begin
  Result := IsSameInternal(ATemplate);
  if Result then begin
    Result :=
      (FTextColor = ATemplate.TextColor) and
      (FTextBgColor = ATemplate.TextBgColor) and
      (FFontSize = ATemplate.FontSize) and
      (FMarkerSize = ATemplate.MarkerSize) and
      (FPic = ATemplate.Pic);
  end;
end;

{ TMarkTemplateLine }

constructor TMarkTemplateLine.Create(
  const ACategoryDb: IMarkCategoryDBSmlInternal;
  const ANameGenerator: IMarkNameGenerator;
  ACategoryId: Integer;
  ALineColor: TColor32;
  ALineWidth: Integer
);
begin
  inherited Create(ACategoryDb, ANameGenerator, ACategoryId);
  FLineColor := ALineColor;
  FLineWidth := ALineWidth;
end;

function TMarkTemplateLine.GetLineColor: TColor32;
begin
  Result := FLineColor;
end;

function TMarkTemplateLine.GetLineWidth: Integer;
begin
  Result := FLineWidth;
end;

function TMarkTemplateLine.IsSame(const ATemplate: IMarkTemplateLine): Boolean;
begin
  Result := IsSameInternal(ATemplate);
  if Result then begin
    Result :=
      (FLineColor = ATemplate.LineColor) and
      (FLineWidth = ATemplate.LineWidth);
  end;
end;

{ TMarkTemplatePoly }

constructor TMarkTemplatePoly.Create(
  const ACategoryDb: IMarkCategoryDBSmlInternal;
  const ANameGenerator: IMarkNameGenerator;
  ACategoryId: Integer;
  ABorderColor, AFillColor: TColor32;
  ALineWidth: Integer
);
begin
  inherited Create(ACategoryDb, ANameGenerator, ACategoryId);
  FBorderColor := ABorderColor;
  FFillColor := AFillColor;
  FLineWidth := ALineWidth;
end;

function TMarkTemplatePoly.GetBorderColor: TColor32;
begin
  Result := FBorderColor;
end;

function TMarkTemplatePoly.GetFillColor: TColor32;
begin
  Result := FFillColor;
end;

function TMarkTemplatePoly.GetLineWidth: Integer;
begin
  Result := FLineWidth;
end;

function TMarkTemplatePoly.IsSame(const ATemplate: IMarkTemplatePoly): Boolean;
begin
  Result := IsSameInternal(ATemplate);
  if Result then begin
    Result :=
      (FBorderColor = ATemplate.BorderColor) and
      (FFillColor = ATemplate.FillColor) and
      (FLineWidth = ATemplate.LineWidth);
  end;
end;

end.
