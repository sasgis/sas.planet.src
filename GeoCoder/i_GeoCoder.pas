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

unit i_GeoCoder;

interface

uses
  t_GeoTypes,
  i_NotifierOperation,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_LocalCoordConverter;

type
  IGeoCodeResult = interface(IVectorItemSubset)
    ['{C90929AD-3A6C-4906-A554-E1DA363ED060}']
    function GetSearchText: string;
    function GetResultCode: Integer;
    function GetMessage: string;
  end;

  IGeoCodePlacemarkInfo = interface(IVectorDataItemMainInfo)
    ['{744CAB70-0466-433A-AF57-00BD5AFD9F45}']
    function GetAccuracy: Integer;
  end;

  IGeoCodePlacemarkFactory = interface
    ['{2ACD5E56-87C4-4A48-BBDD-055D0803C10C}']
    function Build(
      const APoint: TDoublePoint;
      const AAddress: string;
      const ADesc: string;
      const AFullDesc: string;
      const AAccuracy: Integer
    ): IVectorDataItemPoint;
  end;

  IGeoCoder = interface
    ['{D9293293-080A-44B7-92F8-3093D35A551B}']
    function GetLocations(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASearch: string;
      const ALocalConverter: ILocalCoordConverter
    ): IGeoCodeResult;
  end;

implementation

end.
