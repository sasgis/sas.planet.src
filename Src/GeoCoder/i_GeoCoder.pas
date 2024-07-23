{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit i_GeoCoder;

interface

uses
  t_GeoTypes,
  i_NotifierOperation,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_LocalCoordConverter;

const
  CGeoCodeExceptionResultCode = -1;
  CGeoCodeDownloadErrorResultCode = -2;
  CGeoCodeNoInternetConnectionResultCode = -3;
  CGeoCodeNotFoundResultCode = -4;

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
    ): IVectorDataItem;
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
