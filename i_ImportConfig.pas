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

unit i_ImportConfig;

interface

uses
  i_MarkTemplate,
  i_MarksDb;

type
  IImportConfig = interface
    ['{95479381-A0D7-4FE3-86FB-11C5ED532FD2}']
    function GetTemplateNewPoint: IMarkTemplatePoint;
    property TemplateNewPoint: IMarkTemplatePoint read GetTemplateNewPoint;

    function GetTemplateNewLine: IMarkTemplateLine;
    property TemplateNewLine: IMarkTemplateLine read GetTemplateNewLine;

    function GetTemplateNewPoly: IMarkTemplatePoly;
    property TemplateNewPoly: IMarkTemplatePoly read GetTemplateNewPoly;
  end;

implementation

end.
