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

unit i_GeoCoder;

interface

uses
  ActiveX,
  i_NotifierOperation,
  i_VectorDataItemSimple,
  i_LocalCoordConverter;

type
  IGeoCodeResult = interface
    ['{C90929AD-3A6C-4906-A554-E1DA363ED060}']
    function GetSearchText: WideString; safecall;
    function GetResultCode: Integer; safecall;
    function GetMessage: WideString; safecall;
    function GetPlacemarks: IEnumUnknown; safecall;
    function GetPlacemarksCount: integer; safecall;
  end;

  IGeoCodePlacemark = interface(IVectorDataItemPoint)
    ['{744CAB70-0466-433A-AF57-00BD5AFD9F45}']
    function GetAccuracy: Integer; safecall;
  end;

  IGeoCoder = interface
    ['{D9293293-080A-44B7-92F8-3093D35A551B}']
    function GetLocations(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IGeoCodeResult; safecall;
  end;

implementation

end.
