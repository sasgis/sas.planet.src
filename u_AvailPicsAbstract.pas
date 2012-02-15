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

unit u_AvailPicsAbstract;

interface

uses
  Windows,
  SysUtils,
  Classes,
  i_LocalCoordConverter,
  t_GeoTypes;

type
  // for node
  //TAvailImageItemParams = class(TStringList)
  //public
    // only base params - others as items
    //Image_Date: String;
    //Image_Id: String;
    //tid:string;
    //date:string;
    //provider:string;
    //color:string;
    //resolution:string;
  //end;

  // to add items to form
  TAddAvailImageItemProc = function (Sender: TObject;
                                     const ADate: String;
                                     const AId: String;
                                     var AParams: TStrings): Boolean of object;

  PAvailPicsTileInfo = ^TAvailPicsTileInfo;
  TAvailPicsTileInfo = record
    // common
    AddImageProc: TAddAvailImageItemProc;
    LonLat: TDoublePoint;
    Zoom: Byte;
    // for NMC
    TilePos: TPoint;
    // for DG
    mpp: Extended;
    hi,wi: Integer;
  end;

  TAvailPicsAbstract = class(TObject)
  protected
    FTileInfoPtr: PAvailPicsTileInfo;
    FLocalConverter: ILocalCoordConverter;
  public
    constructor Create(const ATileInfoPtr: PAvailPicsTileInfo;
                       const ALocalConverter: ILocalCoordConverter);
    destructor Destroy; override;

    function ContentType: String; virtual; abstract;

    // parse response from server, returns number of added items
    function ParseResponse(const AStream: TMemoryStream): Integer; virtual; abstract;

    // get full link
    function LinkToImages: String; virtual; abstract;
  end;

  TAvailPicsByKey = class(TAvailPicsAbstract)
  protected
    // TODO: obtain key online or get it from zmp
    FDefaultKey: String;
  end;

implementation

{ TAvailPicsAbstract }

constructor TAvailPicsAbstract.Create(const ATileInfoPtr: PAvailPicsTileInfo;
                                      const ALocalConverter: ILocalCoordConverter);
begin
  FTileInfoPtr := ATileInfoPtr;
  FLocalConverter := ALocalConverter;
end;

destructor TAvailPicsAbstract.Destroy;
begin
  FLocalConverter:=nil;
  inherited;
end;

end.
