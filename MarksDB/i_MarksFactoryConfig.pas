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

unit i_MarksFactoryConfig;

interface

uses
  GR32,
  i_ConfigDataElement,
  i_MarkPicture,
  i_MarkCategory,
  i_MarkNameGenerator,
  i_MarkTemplate;

type
  IMarkPointTemplateConfig = interface(IConfigDataElement)
    ['{B796934A-83FE-4E8A-B69D-11237690AA23}']
    function CreateTemplate(
      const APic: IMarkPicture;
      const ACategory: ICategory;
      ATextColor, ATextBgColor: TColor32;
      AFontSize, AMarkerSize: Integer
    ): IMarkTemplatePoint;

    function GetMarkPictureList: IMarkPictureList;
    property MarkPictureList: IMarkPictureList read GetMarkPictureList;

    function GetDefaultTemplate: IMarkTemplatePoint;
    procedure SetDefaultTemplate(const AValue: IMarkTemplatePoint);
    property DefaultTemplate: IMarkTemplatePoint read GetDefaultTemplate write SetDefaultTemplate;

    function GetNameGenerator: IMarkNameGenerator;
  end;

  IMarkLineTemplateConfig = interface(IConfigDataElement)
    ['{0F7596F4-1BA2-4581-9509-77627F50B1AF}']
    function CreateTemplate(
      const ACategory: ICategory;
      ALineColor: TColor32;
      ALineWidth: Integer
    ): IMarkTemplateLine;

    function GetDefaultTemplate: IMarkTemplateLine;
    procedure SetDefaultTemplate(const AValue: IMarkTemplateLine);
    property DefaultTemplate: IMarkTemplateLine read GetDefaultTemplate write SetDefaultTemplate;

    function GetNameGenerator: IMarkNameGenerator;
  end;

  IMarkPolyTemplateConfig = interface(IConfigDataElement)
    ['{149D8DC1-7848-4D34-ABCA-2B7F8D3A22EF}']
    function CreateTemplate(
      const ACategory: ICategory;
      ABorderColor: TColor32;
      AFillColor: TColor32;
      ALineWidth: Integer
    ): IMarkTemplatePoly;

    function GetDefaultTemplate: IMarkTemplatePoly;
    procedure SetDefaultTemplate(const AValue: IMarkTemplatePoly);
    property DefaultTemplate: IMarkTemplatePoly read GetDefaultTemplate write SetDefaultTemplate;

    function GetNameGenerator: IMarkNameGenerator;
  end;


  IMarksFactoryConfig = interface(IConfigDataElement)
    ['{9CC0FDE0-44B2-443D-8856-ED7263F0F8BF}']
    function GetPointTemplateConfig: IMarkPointTemplateConfig;
    property PointTemplateConfig: IMarkPointTemplateConfig read GetPointTemplateConfig;

    function GetLineTemplateConfig: IMarkLineTemplateConfig;
    property LineTemplateConfig: IMarkLineTemplateConfig read GetLineTemplateConfig;

    function GetPolyTemplateConfig: IMarkPolyTemplateConfig;
    property PolyTemplateConfig: IMarkPolyTemplateConfig read GetPolyTemplateConfig;
  end;

implementation

end.
