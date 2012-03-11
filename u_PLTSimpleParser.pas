{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_PLTSimpleParser;

interface

uses
  Classes,
  SysUtils,
  t_GeoTypes,
  i_BinaryData,
  i_VectorItmesFactory,
  i_VectorDataLoader,
  i_DoublePointsAggregator,
  i_HtmlToHintTextConverter,
  i_InternalPerformanceCounter,
  i_VectorDataItemSimple;

type
  TPLTSimpleParser = class(TInterfacedObject, IVectorDataLoader)
  private
    FFactory: IVectorItmesFactory;
    FLoadStreamCounter: IInternalPerformanceCounter;
    FHintConverter: IHtmlToHintTextConverter;
    procedure ParseStringList(AStringList: TStringList; APointsAggregator: IDoublePointsAggregator);
    function GetWord(Str, Smb: string; WordNmbr: Byte): string;
  protected
    function LoadFromStream(AStream: TStream): IVectorDataItemList;
    function Load(AData: IBinaryData): IVectorDataItemList; virtual;
  public
    constructor Create(
      AFactory: IVectorItmesFactory;
      AHintConverter: IHtmlToHintTextConverter;
      APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  u_StreamReadOnlyByBinaryData,
  u_VectorDataItemList,
  u_VectorDataItemPolygon,
  u_DoublePointsAggregator,
  u_GeoFun,
  u_GeoToStr;

constructor TPLTSimpleParser.Create(
  AFactory: IVectorItmesFactory;
  AHintConverter: IHtmlToHintTextConverter;
  APerfCounterList: IInternalPerformanceCounterList
);
begin
  FFactory := AFactory;
  FHintConverter := AHintConverter;
  FLoadStreamCounter := APerfCounterList.CreateAndAddNewCounter('LoadPltStream');
end;

function TPLTSimpleParser.Load(AData: IBinaryData): IVectorDataItemList;
var
  VStream: TStreamReadOnlyByBinaryData;
begin
  Result := nil;
  VStream := TStreamReadOnlyByBinaryData.Create(AData);
  try
    Result := LoadFromStream(VStream);
  finally
    VStream.Free;
  end;
end;

function TPLTSimpleParser.LoadFromStream(AStream: TStream): IVectorDataItemList;
var
  pltstr: TStringList;
  trackname: string;
  VList: IInterfaceList;
  VItem: IVectorDataItemSimple;
  VPointsAggregator: IDoublePointsAggregator;
begin
  Result := nil;
  pltstr:=TStringList.Create;
  try
    pltstr.LoadFromStream(AStream);
    if pltstr.Count > 7 then begin
      VPointsAggregator := TDoublePointsAggregator.Create;
      ParseStringList(pltstr, VPointsAggregator);
      if VPointsAggregator.Count > 0 then begin
        trackname:=GetWord(pltstr[4], ',', 4);
        VItem :=
          TVectorDataItemPath.Create(
            FHintConverter,
            trackname,
            '',
            FFactory.CreateLonLatPath(VPointsAggregator.Points, VPointsAggregator.Count)
          );
        VList := TInterfaceList.Create;
        VList.Add(VItem);
        Result := TVectorDataItemList.Create(VList);
      end;
    end;
  finally
    pltstr.Free;
  end;
end;

procedure TPLTSimpleParser.ParseStringList(
  AStringList: TStringList;
  APointsAggregator: IDoublePointsAggregator
);
var
  i,j:integer;
  str: string;
  VPoint: TDoublePoint;
  VValidPoint: Boolean;
begin
  for i:=6 to AStringList.Count-1 do begin
    try
      j:=1;
      str:=AStringList[i];
      while j<length(str) do begin
        if str[j]=' ' then begin
          delete(str,j,1);
        end else begin
          inc(j);
        end;
      end;
      if (GetWord(AStringList[i], ',', 3)='1') and (i>6) then begin
        VPoint := CEmptyDoublePoint;
        APointsAggregator.Add(VPoint);
      end;
      VValidPoint := True;
      try
        VPoint.y := str2r(GetWord(str, ',', 1));
        VPoint.x := str2r(GetWord(str, ',', 2));
      except
        VValidPoint := False;
      end;
      if VValidPoint then begin
        APointsAggregator.Add(VPoint);
      end;
    except
    end;
  end;
end;

function TPLTSimpleParser.GetWord(Str, Smb: string; WordNmbr: Byte): string;
var SWord: string;
    StrLen, N: Byte;
begin
  StrLen := SizeOf(Str);
  N := 1;
  while ((WordNmbr >= N) and (StrLen <> 0)) do
  begin
    StrLen := System.Pos(Smb, str);
    if StrLen <> 0 then
    begin
      SWord := Copy(Str, 1, StrLen - 1);
      Delete(Str, 1, StrLen);
      Inc(N);
    end
    else SWord := Str;
  end;
  if WordNmbr <= N then Result := SWord
                   else Result := '';
end;

end.
