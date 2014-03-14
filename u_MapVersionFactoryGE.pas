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

unit u_MapVersionFactoryGE deprecated;

interface

uses
  t_Hash,
  i_MapVersionInfo,
  i_MapVersionFactory,
  u_BaseInterfacedObject;

type
  IMapVersionFactoryGEInternal = interface(IMapVersionFactory)
    ['{F5A6FBE0-D633-4BCB-999A-BB0428CB1EF2}']
    function CreateByGE(
      const AVer: Word;
      const AGEServer: String;
      const ATileDate: String
    ): IMapVersionInfo;
  end;

  TMapVersionFactoryGE = class(TBaseInterfacedObject, IMapVersionFactory, IMapVersionFactoryGEInternal)
  private
    function CreateByStoreString(const AValue: string): IMapVersionInfo;
    function CreateByMapVersion(const AValue: IMapVersionInfo): IMapVersionInfo;
    function IsSameFactoryClass(const AMapVersionFactory: IMapVersionFactory): Boolean;
    { IMapVersionFactoryGEInternal }
    function CreateByGE(
      const AVer: Word;
      const AGEServer: String;
      const ATileDate: String
    ): IMapVersionInfo;
  end;

implementation

uses
  SysUtils,
  i_MapVersionInfoGE;

type
  TMapVersionInfoGE = class(TBaseInterfacedObject, IMapVersionInfo, IMapVersionInfoGE)
  private
    FHash: THashValue;
    FVer: Word;
    FGEServer: String;
    FTileDate: String;
  private
    function GetHash: THashValue;
    function GetUrlString: string;
    function GetStoreString: string;
    function GetCaption: string;
    function GetVer: Word;
    function GetGEServer: String;
    function GetTileDate: String;

    function IsSame(const AValue: IMapVersionInfo): Boolean;
  public
    constructor Create(
      const AHash: THashValue;
      const AVer: Word;
      const AGEServer: String;
      const ATileDate: String
    );
  end;

{ TMapVersionInfoGE }

constructor TMapVersionInfoGE.Create(
  const AHash: THashValue;
  const AVer: Word;
  const AGEServer: String;
  const ATileDate: String
);
begin
  inherited Create;
  FHash := AHash;
  FVer := AVer;
  FGEServer := AGEServer;
  FTileDate := ATileDate;
end;

function TMapVersionInfoGE.GetCaption: string;
begin
  // very simple!
  Result := GetStoreString;
end;

function TMapVersionInfoGE.GetGEServer: String;
begin
  Result := FGEServer;
end;

function TMapVersionInfoGE.GetHash: THashValue;
begin
  Result := FHash;
end;

function TMapVersionInfoGE.GetStoreString: string;
begin
  // full format: date\ver[GEServer]
  Result := FTileDate;

  // Ver
  if (FVer <> 0) then begin
    if (0 < Length(Result)) then begin
      Result := Result + '\';
    end;
    Result := Result + IntToStr(FVer);
  end;

  // GEServer
  if (0 < Length(FGEServer)) then begin
    Result := Result + '[' + FGEServer + ']';
  end;
end;

function TMapVersionInfoGE.GetTileDate: String;
begin
  Result := FTileDate;
end;

function TMapVersionInfoGE.GetUrlString: string;
begin
  Result := '';
end;

function TMapVersionInfoGE.GetVer: Word;
begin
  Result := FVer;
end;

function TMapVersionInfoGE.IsSame(const AValue: IMapVersionInfo): Boolean;
var
  VVersionGE: IMapVersionInfoGE;
begin
  if AValue = nil then begin
    Result := False;
  end else begin
    if (AValue = IMapVersionInfo(Self)) or (AValue = IMapVersionInfoGE(Self)) then begin
      Result := True;
    end else begin
      if (FHash <> 0) and (AValue.Hash <> 0) and (FHash <> AValue.Hash) then begin
        Result := False;
      end else begin
        if Supports(AValue, IMapVersionInfoGE, VVersionGE) then begin
          Result :=
            (FGEServer = VVersionGE.GEServer) and
            (FVer = VVersionGE.Ver) and
            (FTileDate = VVersionGE.TileDate);
        end else begin
          Result := AValue.StoreString = GetStoreString;
        end;
      end;
    end;
  end;
end;

{ TMapVersionFactoryGE }

function TMapVersionFactoryGE.CreateByGE(
  const AVer: Word;
  const AGEServer: String;
  const ATileDate: String
): IMapVersionInfo;
begin
  Result := TMapVersionInfoGE.Create(0, AVer, AGEServer, ATileDate);
end;

function TMapVersionFactoryGE.CreateByMapVersion(
  const AValue: IMapVersionInfo
): IMapVersionInfo;
begin
  Result := nil;
  if not Supports(AValue, IMapVersionInfoGE, Result) then begin
    if AValue <> nil then begin
      Result := CreateByStoreString(AValue.StoreString);
    end else begin
      Result := CreateByStoreString('');
    end;
  end;
end;

function TMapVersionFactoryGE.CreateByStoreString(
  const AValue: string
): IMapVersionInfo;

  function _StrToWord(
  const ASrc: String;
  var w: Word
  ): Boolean;
  var
    v: Integer;
  begin
    Result := FALSE;
    if (0 < Length(ASrc)) then begin
      if TryStrToInt(Trim(ASrc), v) then begin
        if (v > 0) and (v <= $FFFF) then begin
          w := v;
          Inc(Result);
        end;
      end;
    end;
  end;

  procedure _StrToByte(
  const ASrc: String;
  var b: Byte
  );
  var
    v: Integer;
  begin
    if (0 < Length(ASrc)) then begin
      if TryStrToInt(Trim(ASrc), v) then begin
        if (v > 0) and (v <= $FF) then begin
          b := v;
        end;
      end;
    end;
  end;

var
  VVer: Word;
  VGEServer: String;
  VTileDate: String;
  VPos: Integer;
begin
  // full format: date\ver[GEServer]
  // allow any combinations (ver[GEServer], date[GEServer], date\ver,...)
  VVer := 0;

  // get '['
  VPos := System.Pos('[', AValue);
  if (VPos > 0) then begin
    // with GEServer
    VTileDate := System.Copy(AValue, 1, (VPos - 1));
    VGEServer := System.Copy(AValue, (VPos + 1), Length(AValue));
    VPos := System.Pos(']', VGEServer);
    if (VPos > 0) then begin
      SetLength(VGEServer, (VPos - 1));
    end;
  end else begin
    // no GEServer - just date\ver
    VGEServer := '';
    VTileDate := AValue;
  end;

  // parse TileDate (as 'date\ver')
  VPos := System.Pos('\', VTileDate);
  if (VPos > 0) then begin
    // both TileDate and Ver
    _StrToWord(System.Copy(VTileDate, (VPos + 1), Length(VTileDate)), VVer);
    SetLength(VTileDate, (VPos - 1));
  end else begin
    // TileDate OR Ver - check it as int
    if _StrToWord(VTileDate, VVer) then begin
      // fill Ver - clear VTileDate
      VTileDate := '';
    end;
  end;

  Result := TMapVersionInfoGE.Create(0, VVer, VGEServer, VTileDate);
end;

function TMapVersionFactoryGE.IsSameFactoryClass(const AMapVersionFactory: IMapVersionFactory): Boolean;
var V: IMapVersionFactoryGEInternal;
begin
  if (nil=AMapVersionFactory) then begin
    Result := False;
  end else begin
    Result := Supports(AMapVersionFactory, IMapVersionFactoryGEInternal, V);
  end;
end;

end.
