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

unit i_MarksGUIConfig;

interface

uses
  i_ConfigDataElement;

type
  IMarksGUIConfig = interface(IConfigDataElement)
    ['{5F3F4E37-FF16-4DC6-8A37-ADB30868C2DA}']
    function GetIsAddTypeToCaption: Boolean;
    procedure SetIsAddTypeToCaption(AValue: Boolean);
    property IsAddTypeToCaption: Boolean read GetIsAddTypeToCaption write SetIsAddTypeToCaption;

    function GetIsAddTimeToDescription: Boolean;
    procedure SetIsAddTimeToDescription(AValue: Boolean);
    property IsAddTimeToDescription: Boolean read GetIsAddTimeToDescription write SetIsAddTimeToDescription;
  end;

implementation

end.
