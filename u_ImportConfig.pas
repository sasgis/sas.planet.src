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

unit u_ImportConfig;

interface

uses
  i_Category,
  i_MarkTemplate,
  i_ImportConfig,
  u_BaseInterfacedObject;

type
  TImportPointParams = class(TBaseInterfacedObject, IImportPointParams)
  private
    FTemplate: IMarkTemplatePoint;
    FIsForceTextColor: Boolean;
    FIsForceTextBgColor: Boolean;
    FIsForceFontSize: Boolean;
    FIsForceMarkerSize: Boolean;
    FIsForcePicName: Boolean;
  private
    function GetTemplate: IMarkTemplatePoint;
    function GetIsForceTextColor: Boolean;
    function GetIsForceTextBgColor: Boolean;
    function GetIsForceFontSize: Boolean;
    function GetIsForceMarkerSize: Boolean;
    function GetIsForcePicName: Boolean;
  public
    constructor Create(
      const ATemplate: IMarkTemplatePoint;
      const AIsForceTextColor: Boolean;
      const AIsForceTextBgColor: Boolean;
      const AIsForceFontSize: Boolean;
      const AIsForceMarkerSize: Boolean;
      const AIsForcePicName: Boolean
    );
  end;

  TImportLineParams = class(TBaseInterfacedObject, IImportLineParams)
  private
    FTemplate: IMarkTemplateLine;
    FIsForceLineColor: Boolean;
    FIsForceLineWidth: Boolean;
  private
    function GetTemplate: IMarkTemplateLine;
    function GetIsForceLineColor: Boolean;
    function GetIsForceLineWidth: Boolean;
  public
    constructor Create(
      const ATemplate: IMarkTemplateLine;
      const AIsForceLineColor: Boolean;
      const AIsForceLineWidth: Boolean
    );
  end;

  TImportPolyParams = class(TBaseInterfacedObject, IImportPolyParams)
  private
    FTemplate: IMarkTemplatePoly;
    FIsForceLineColor: Boolean;
    FIsForceLineWidth: Boolean;
    FIsForceFillColor: Boolean;
  private
    function GetTemplate: IMarkTemplatePoly;
    function GetIsForceLineColor: Boolean;
    function GetIsForceLineWidth: Boolean;
    function GetIsForceFillColor: Boolean;
  public
    constructor Create(
      const ATemplate: IMarkTemplatePoly;
      const AIsForceLineColor: Boolean;
      const AIsForceLineWidth: Boolean;
      const AIsForceFillColor: Boolean
    );
  end;

  TImportConfig = class(TBaseInterfacedObject, IImportConfig)
  private
    FRootCategory: ICategory;
    FTemplateNewPoint: IMarkTemplatePoint;
    FTemplateNewLine: IMarkTemplateLine;
    FTemplateNewPoly: IMarkTemplatePoly;
  private
    function GetRootCategory: ICategory;
    function GetTemplateNewPoint: IMarkTemplatePoint;
    function GetTemplateNewLine: IMarkTemplateLine;
    function GetTemplateNewPoly: IMarkTemplatePoly;
  public
    constructor Create(
      const ARootCategory: ICategory;
      const ATemplateNewPoint: IMarkTemplatePoint;
      const ATemplateNewLine: IMarkTemplateLine;
      const ATemplateNewPoly: IMarkTemplatePoly
    );
  end;

implementation

{ TImportConfig }

constructor TImportConfig.Create(
  const ARootCategory: ICategory;
  const ATemplateNewPoint: IMarkTemplatePoint;
  const ATemplateNewLine: IMarkTemplateLine;
  const ATemplateNewPoly: IMarkTemplatePoly
);
begin
  inherited Create;
  FRootCategory := ARootCategory;
  FTemplateNewPoint := ATemplateNewPoint;
  FTemplateNewLine := ATemplateNewLine;
  FTemplateNewPoly := ATemplateNewPoly;
end;

function TImportConfig.GetRootCategory: ICategory;
begin
  Result := FRootCategory;
end;

function TImportConfig.GetTemplateNewLine: IMarkTemplateLine;
begin
  Result := FTemplateNewLine;
end;

function TImportConfig.GetTemplateNewPoint: IMarkTemplatePoint;
begin
  Result := FTemplateNewPoint;
end;

function TImportConfig.GetTemplateNewPoly: IMarkTemplatePoly;
begin
  Result := FTemplateNewPoly;
end;

{ TImportPointParams }

constructor TImportPointParams.Create(
  const ATemplate: IMarkTemplatePoint;
  const AIsForceTextColor: Boolean;
  const AIsForceTextBgColor: Boolean;
  const AIsForceFontSize: Boolean;
  const AIsForceMarkerSize: Boolean;
  const AIsForcePicName: Boolean
);
begin
  inherited Create;
  FTemplate := ATemplate;
  FIsForceTextColor := AIsForceTextColor;
  FIsForceTextBgColor := AIsForceTextBgColor;
  FIsForceFontSize := AIsForceFontSize;
  FIsForceMarkerSize := AIsForceMarkerSize;
  FIsForcePicName := AIsForcePicName;
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

function TImportPointParams.GetTemplate: IMarkTemplatePoint;
begin
  Result := FTemplate;
end;

{ TImportLineParams }

constructor TImportLineParams.Create(
  const ATemplate: IMarkTemplateLine;
  const AIsForceLineColor: Boolean;
  const AIsForceLineWidth: Boolean
);
begin
  inherited Create;
  FTemplate := ATemplate;
  FIsForceLineColor := AIsForceLineWidth;
  FIsForceLineWidth := AIsForceLineWidth;
end;

function TImportLineParams.GetIsForceLineColor: Boolean;
begin
  Result := FIsForceLineColor;
end;

function TImportLineParams.GetIsForceLineWidth: Boolean;
begin
  Result := FIsForceLineWidth;
end;

function TImportLineParams.GetTemplate: IMarkTemplateLine;
begin
  Result := FTemplate;
end;

{ TImportPolyParams }

constructor TImportPolyParams.Create(
  const ATemplate: IMarkTemplatePoly;
  const AIsForceLineColor: Boolean;
  const AIsForceLineWidth: Boolean;
  const AIsForceFillColor: Boolean
);
begin
  inherited Create;
  FTemplate := ATemplate;
  FIsForceLineColor := AIsForceLineWidth;
  FIsForceLineWidth := AIsForceLineWidth;
  FIsForceFillColor := AIsForceFillColor;
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

function TImportPolyParams.GetTemplate: IMarkTemplatePoly;
begin
  Result := FTemplate;
end;

end.
