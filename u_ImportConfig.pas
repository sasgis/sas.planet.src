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
  i_MarkTemplate,
  i_ImportConfig,
  i_MarksDb;

type
  TImportConfig = class(TInterfacedObject, IImportConfig)
  private
    FMarkDB: IMarksDb;
    FTemplateNewPoint: IMarkTemplatePoint;
    FTemplateNewLine: IMarkTemplateLine;
    FTemplateNewPoly: IMarkTemplatePoly;
  protected
    function GetMarkDB: IMarksDb;
    function GetTemplateNewPoint: IMarkTemplatePoint;
    function GetTemplateNewLine: IMarkTemplateLine;
    function GetTemplateNewPoly: IMarkTemplatePoly;
  public
    constructor Create(
      AMarkDB: IMarksDb;
      ATemplateNewPoint: IMarkTemplatePoint;
      ATemplateNewLine: IMarkTemplateLine;
      ATemplateNewPoly: IMarkTemplatePoly
    );
  end;

implementation

{ TImportConfig }

constructor TImportConfig.Create(
  AMarkDB: IMarksDb;
  ATemplateNewPoint: IMarkTemplatePoint;
  ATemplateNewLine: IMarkTemplateLine;
  ATemplateNewPoly: IMarkTemplatePoly
);
begin
  FMarkDB := AMarkDB;
  FTemplateNewPoint := ATemplateNewPoint;
  FTemplateNewLine := ATemplateNewLine;
  FTemplateNewPoly := ATemplateNewPoly;
end;

function TImportConfig.GetMarkDB: IMarksDb;
begin
  Result := FMarkDB;
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

end.
