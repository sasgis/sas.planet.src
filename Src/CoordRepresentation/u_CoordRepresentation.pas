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

unit u_CoordRepresentation;

interface

uses
  Types,
  SysUtils,
  t_CoordRepresentation,
  i_CoordRepresentationConfig;

type
  TCoordSysTypeCaption = array [TCoordSysType] of string;

function GetCoordSysTypeCaption: TCoordSysTypeCaption;
function GetCoordSysTypeCaptionShort: TCoordSysTypeCaption;
function GetUPSCoordSysTypeCaptionShort: string;

function CoordSysTypeToInteger(const AValue: TCoordSysType): Integer;
function IntegerToCoordSysType(const AValue: Integer): TCoordSysType;

procedure SetCoordSysType(
  const AConfig: ICoordRepresentationConfig;
  const AIndex: Integer
);

//-----------------------------------------------------------------------------

function GeogCoordShowFormatToInteger(const AValue: TGeogCoordShowFormat): Integer;
function IntegerToGeogCoordShowFormat(const AValue: Integer): TGeogCoordShowFormat;

//-----------------------------------------------------------------------------

function ProjCoordShowFormatToInteger(const AValue: TProjCoordShowFormat): Integer;
function IntegerToProjCoordShowFormat(const AValue: Integer): TProjCoordShowFormat;

//-----------------------------------------------------------------------------

function GetCoordShowFormatCaptions(
  const AConfig: ICoordRepresentationConfigStatic;
  out AItems: TStringDynArray;
  out AActiveItemIndex: Integer
): Boolean;

procedure SetCoordShowFormat(
  const AConfig: ICoordRepresentationConfig;
  const AIndex: Integer
);

implementation

uses
  gnugettext;

const
  CCoordSysTypeId: array[TCoordSysType] of Integer = (
    0, 3, 4, 1, 2, 11, 12
  );

function GetCoordSysTypeCaption: TCoordSysTypeCaption;
begin
  Result[cstWGS84]  := _('WGS 84 / Geographic');

  Result[cstUTM]    := _('WGS 84 / UTM (6 degree zones)');
  Result[cstMGRS]   := 'WGS 84 / MGRS';

  Result[cstSK42]   := _('SK-42 (Pulkovo-1942) / Geographic');
  Result[cstSK42GK] := _('SK-42 / Gauss-Kruger (6 degree zones)');

  Result[cstGSK2011]   := _('GSK-2011 / Geographic');
  Result[cstGSK2011GK] := _('GSK-2011 / Gauss-Kruger (6 degree zones)');
end;

function GetCoordSysTypeCaptionShort: TCoordSysTypeCaption;
begin
  Result[cstWGS84]  := 'WGS 84';

  Result[cstUTM]    := 'WGS 84 / UTM';
  Result[cstMGRS]   := 'WGS 84 / MGRS';

  Result[cstSK42]   := _('SK-42');
  Result[cstSK42GK] := _('SK-42 / GK');

  Result[cstGSK2011]   := _('GSK-2011');
  Result[cstGSK2011GK] := _('GSK-2011 / GK');
end;

function GetUPSCoordSysTypeCaptionShort: string;
begin
  Result := 'WGS 84 / UPS';
end;

function CoordSysTypeToInteger(const AValue: TCoordSysType): Integer;
begin
  Result := CCoordSysTypeId[AValue];
end;

function IntegerToCoordSysType(const AValue: Integer): TCoordSysType;
var
  I: TCoordSysType;
begin
  for I := Low(TCoordSysType) to High(TCoordSysType) do begin
    if CCoordSysTypeId[I] = AValue then begin
      Result := I;
      Exit;
    end;
  end;
  Result := Low(TCoordSysType);
end;

procedure SetCoordSysType(
  const AConfig: ICoordRepresentationConfig;
  const AIndex: Integer
);
var
  I: Integer;
  VType: TCoordSysType;
begin
  I := 0;
  for VType := Low(TCoordSysType) to High(TCoordSysType) do begin
    if I = AIndex then begin
      AConfig.CoordSysType := VType;
      Exit;
    end;
    Inc(I);
  end;
end;

//-----------------------------------------------------------------------------

type
  TGeogCoordShowFormatCaption = array [TGeogCoordShowFormat] of string;
  TProjCoordShowFormatCaption = array [TProjCoordShowFormat] of string;
  TMgrsCoordShowFormatCaption = array [TMgrsCoordShowFormat] of string;

function GetGeogCoordShowFormatCaption: TGeogCoordShowFormatCaption;
begin
  Result[dshCharDegrMinSec] := _('WS deg.min.sec. (W12°12''12.1234")');
  Result[dshCharDegrMin]    := _('WS deg.min. (W12°12.123456'')');
  Result[dshCharDegr]       := _('WS deg. (W12.12345678°)');
  Result[dshCharDegr2]      := _('WS deg. (W12.12345678)');

  Result[dshSignDegrMinSec] := _('-- deg.min.sec. (-12°12''12.1234")');
  Result[dshSignDegrMin]    := _('-- deg.min. (-12°12.1234'')');
  Result[dshSignDegr]       := _('-- deg. (-12.12345678°)');
  Result[dshSignDegr2]      := _('-- deg. (-12.12345678)');
end;

const
  CGeogCoordShowFormatId: array[TGeogCoordShowFormat] of Integer = (
    0, 1, 2, 21, 3, 4, 5, 51
  );

function GeogCoordShowFormatToInteger(const AValue: TGeogCoordShowFormat): Integer;
begin
  Result := CGeogCoordShowFormatId[AValue];
end;

function IntegerToGeogCoordShowFormat(const AValue: Integer): TGeogCoordShowFormat;
var
  I: TGeogCoordShowFormat;
begin
  for I := Low(TGeogCoordShowFormat) to High(TGeogCoordShowFormat) do begin
    if CGeogCoordShowFormatId[I] = AValue then begin
      Result := I;
      Exit;
    end;
  end;

  Result := Low(TGeogCoordShowFormat);
end;

function GetProjCoordShowFormatCaption: TProjCoordShowFormatCaption;
begin
  Result[psfRoundedToWhole] := '12345';
  Result[psfRoundedToTenth] := '12345.0';
  Result[psfRoundedToHundredths] := '12345.00';
  Result[psfRoundedToThousandths] := '12345.000';
end;

const
  CProjCoordShowFormatId: array[TProjCoordShowFormat] of Integer = (
    0, 1, 2, 3
  );

function ProjCoordShowFormatToInteger(const AValue: TProjCoordShowFormat): Integer;
begin
  Result := Integer(AValue);
end;

function IntegerToProjCoordShowFormat(const AValue: Integer): TProjCoordShowFormat;
var
  I: TProjCoordShowFormat;
begin
  for I := Low(TProjCoordShowFormat) to High(TProjCoordShowFormat) do begin
    if CProjCoordShowFormatId[I] = AValue then begin
      Result := I;
      Exit;
    end;
  end;

  Result := Low(TProjCoordShowFormat);
end;

function GetMgrsCoordShowFormatCaption: TMgrsCoordShowFormatCaption;
begin
  Result[msfSplitted] := '12U PQ 12345 12345';
  Result[msfJoined]   := '12UPQ1234512345';
end;

function GetCoordShowFormatCaptions(
  const AConfig: ICoordRepresentationConfigStatic;
  out AItems: TStringDynArray;
  out AActiveItemIndex: Integer
): Boolean;

  procedure _GetGeogCaptions;
  var
    I: Integer;
    VFormat: TGeogCoordShowFormat;
    VCaption: TGeogCoordShowFormatCaption;
  begin
    VCaption := GetGeogCoordShowFormatCaption;
    SetLength(AItems, Length(VCaption));

    I := 0;
    for VFormat := Low(TGeogCoordShowFormat) to High(TGeogCoordShowFormat) do begin
      AItems[I] := VCaption[VFormat];
      Inc(I);
    end;

    AActiveItemIndex := Integer(AConfig.GeogCoordShowFormat);
  end;

  procedure _GetProjCaptions;
  var
    I: Integer;
    VFormat: TProjCoordShowFormat;
    VCaption: TProjCoordShowFormatCaption;
  begin
    VCaption := GetProjCoordShowFormatCaption;
    SetLength(AItems, Length(VCaption));

    I := 0;
    for VFormat := Low(TProjCoordShowFormat) to High(TProjCoordShowFormat) do begin
      AItems[I] := VCaption[VFormat];
      Inc(I);
    end;

    AActiveItemIndex := Integer(AConfig.ProjCoordShowFormat);
  end;

  procedure _GetMgrsCaptions;
  var
    I: Integer;
    VFormat: TMgrsCoordShowFormat;
    VCaption: TMgrsCoordShowFormatCaption;
  begin
    VCaption := GetMgrsCoordShowFormatCaption;
    SetLength(AItems, Length(VCaption));

    I := 0;
    for VFormat := Low(TMgrsCoordShowFormat) to High(TMgrsCoordShowFormat) do begin
      AItems[I] := VCaption[VFormat];
      Inc(I);
    end;

    AActiveItemIndex := Integer(AConfig.MgrsCoordShowFormat);
  end;

begin
  AItems := nil;
  AActiveItemIndex := -1;

  case AConfig.CoordSysType of
    cstWGS84, cstSK42, cstGSK2011: begin
      _GetGeogCaptions;
    end;

    cstUTM, cstSK42GK, cstGSK2011GK: begin
      _GetProjCaptions;
    end;

    cstMGRS: begin
      _GetMgrsCaptions;
    end;
  else
    Assert(False);
  end;

  Result := (Length(AItems) > 0) and (AActiveItemIndex >= 0);
end;

procedure SetCoordShowFormat(
  const AConfig: ICoordRepresentationConfig;
  const AIndex: Integer
);
begin
  case AConfig.CoordSysType of
    cstWGS84, cstSK42, cstGSK2011: begin
      AConfig.GeogCoordShowFormat := TGeogCoordShowFormat(AIndex);
    end;

    cstUTM, cstSK42GK, cstGSK2011GK: begin
      AConfig.ProjCoordShowFormat := TProjCoordShowFormat(AIndex);
    end;

    cstMGRS: begin
      AConfig.MgrsCoordShowFormat := TMgrsCoordShowFormat(AIndex);
    end;
  else
    Assert(False);
  end;
end;

end.
