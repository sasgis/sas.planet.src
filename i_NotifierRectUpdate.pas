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

unit i_NotifierRectUpdate;

interface

uses
  Types,
  i_Listener,
  i_Point,
  i_Rect;

type
  INotifierRectUpdate = interface
    ['{67415555-955C-4BC7-BC8F-2F9BCDD0F065}']
    procedure AddListenerByRect(
      const AListener: IListener;
      const ATileRect: TRect
    );
    procedure AddListener(
      const AListener: IListener
    );
    procedure Remove(const AListener: IListener);
  end;

  INotifierRectUpdateInternal = interface
    procedure FullUpdateNotify;
    procedure UpdateNotify(const APoint: IPoint); overload;
    procedure UpdateNotify(const APoint: TPoint); overload;
    procedure RectUpdateNotify(const ARect: IRect); overload;
    procedure RectUpdateNotify(const ARect: TRect); overload;
  end;

implementation

end.
