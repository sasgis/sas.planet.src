{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_InternalBrowserImpl;

interface

uses
  i_DownloadRequest;

const
  CEmptyDocument = 'about:blank';

type
  TOnKeyDown = procedure(Sender: TObject; const AKey: Word; var AHandled: Boolean) of object;
  TOnTitleChange = procedure(Sender: TObject; const AText: string) of object;

  TInternalBrowserImpl = class
  public
    function Initialize: Boolean; virtual; abstract;
    procedure AssignEmptyDocument; virtual; abstract;
    procedure Navigate(const AUrl: string); overload; virtual; abstract;
    procedure Navigate(const ARequest: IDownloadRequest); overload; virtual; abstract;
    function NavigateWait(const AUrl: string; const ATimeOut: Cardinal): Boolean; virtual; abstract;
    procedure SetHtmlText(const AText: string); virtual; abstract;
    procedure Stop; virtual; abstract;
    procedure SetVisible(const AIsVisible: Boolean); virtual; abstract;
  end;

implementation

end.
