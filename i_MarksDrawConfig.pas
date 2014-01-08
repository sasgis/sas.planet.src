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
  t_Hash,
  i_ConfigDataElement;

type
  ICaptionDrawConfigStatic = interface
    ['{745D5905-E954-4976-8697-CF2CD65AD55E}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function GetShowPointCaption: Boolean;
    property ShowPointCaption: Boolean read GetShowPointCaption;

    function GetUseSolidCaptionBackground: Boolean;
    property UseSolidCaptionBackground: Boolean read GetUseSolidCaptionBackground;
  end;

  ICaptionDrawConfig = interface(IConfigDataElement)
    ['{60BDB515-27FF-41D5-87DC-151E21DD0C3D}']
    function GetShowPointCaption: Boolean;
    procedure SetShowPointCaption(AValue: Boolean);
    property ShowPointCaption: Boolean read GetShowPointCaption write SetShowPointCaption;

    function GetUseSolidCaptionBackground: Boolean;
    procedure SetUseSolidCaptionBackground(AValue: Boolean);
    property UseSolidCaptionBackground: Boolean read GetUseSolidCaptionBackground write SetUseSolidCaptionBackground;

    function GetStatic: ICaptionDrawConfigStatic;
  end;

  IMarksDrawOrderConfigStatic = interface
    ['{2BC70BD2-74E8-4063-BB70-03445CBCFD00}']
    function GetUseSimpleDrawOrder: Boolean;
    property UseSimpleDrawOrder: Boolean read GetUseSimpleDrawOrder;

    function GetOverSizeRect: TRect;
    property OverSizeRect: TRect read GetOverSizeRect;
  end;

  IMarksDrawOrderConfig = interface(IConfigDataElement)
    ['{66D58712-23C7-4796-8A5C-6FA3F762686D}']
    function GetUseSimpleDrawOrder: Boolean;
    procedure SetUseSimpleDrawOrder(AValue: Boolean);
    property UseSimpleDrawOrder: Boolean read GetUseSimpleDrawOrder write SetUseSimpleDrawOrder;

    function GetOverSizeRect: TRect;
    procedure SetOverSizeRect(AValue: TRect);
    property OverSizeRect: TRect read GetOverSizeRect write SetOverSizeRect;

    function GetStatic: IMarksDrawOrderConfigStatic;
  end;

  IMarksDrawConfig = interface(IConfigDataElement)
    ['{992DD23C-E0AA-4731-99A9-9049F55DFF6E}']
    function GetCaptionDrawConfig: ICaptionDrawConfig;
    property CaptionDrawConfig: ICaptionDrawConfig read GetCaptionDrawConfig;

    function GetDrawOrderConfig: IMarksDrawOrderConfig;
    property DrawOrderConfig: IMarksDrawOrderConfig read GetDrawOrderConfig;
  end;

implementation

end.
