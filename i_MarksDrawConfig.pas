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

unit i_MarksDrawConfig;

interface

uses
  Types,
  i_ConfigDataElement;

type
  IMarksDrawConfigStatic = interface
    ['{2BC70BD2-74E8-4063-BB70-03445CBCFD00}']
    function GetShowPointCaption: Boolean;
    property ShowPointCaption: Boolean read GetShowPointCaption;

    function GetUseSolidCaptionBackground: Boolean;
    property UseSolidCaptionBackground: Boolean read GetUseSolidCaptionBackground;

    function GetUseSimpleDrawOrder: Boolean;
    property UseSimpleDrawOrder: Boolean read GetUseSimpleDrawOrder;

    function GetOverSizeRect: TRect;
    property OverSizeRect: TRect read GetOverSizeRect;
  end;

  IMarksDrawConfig = interface(IConfigDataElement)
    ['{992DD23C-E0AA-4731-99A9-9049F55DFF6E}']
    function GetShowPointCaption: Boolean;
    procedure SetShowPointCaption(AValue: Boolean);
    property ShowPointCaption: Boolean read GetShowPointCaption write SetShowPointCaption;

    function GetUseSolidCaptionBackground: Boolean;
    procedure SetUseSolidCaptionBackground(AValue: Boolean);
    property UseSolidCaptionBackground: Boolean read GetUseSolidCaptionBackground write SetUseSolidCaptionBackground;

    function GetUseSimpleDrawOrder: Boolean;
    procedure SetUseSimpleDrawOrder(AValue: Boolean);
    property UseSimpleDrawOrder: Boolean read GetUseSimpleDrawOrder write SetUseSimpleDrawOrder;

    function GetOverSizeRect: TRect;
    procedure SetOverSizeRect(AValue: TRect);
    property OverSizeRect: TRect read GetOverSizeRect write SetOverSizeRect;

    function GetStatic: IMarksDrawConfigStatic;
  end;

implementation

end.
