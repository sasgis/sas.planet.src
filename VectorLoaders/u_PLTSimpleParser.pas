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

unit u_PLTSimpleParser;

interface

uses
  Classes,
  t_GeoTypes,
  i_BinaryData,
  i_VectorItemSubsetBuilder,
  i_VectorDataFactory,
  i_VectorItemsFactory,
  i_VectorDataLoader,
  i_VectorItemSubset,
  i_DoublePointsAggregator,
  i_InternalPerformanceCounter,
  i_VectorDataItemSimple,
  u_BaseInterfacedObject;

type
  TPLTSimpleParser = class(TBaseInterfacedObject, IVectorDataLoader)
  private
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FFactory: IVectorItemsFactory;
    FLoadStreamCounter: IInternalPerformanceCounter;
    procedure ParseStringList(
      AStringList: TStringList;
      const APointsAggregator: IDoublePointsAggregator
    );
    function GetWord(
      Str: string;
      const Smb: string;
      WordNmbr: Byte
    ): string;
  private
    function LoadFromStream(
      AStream: TStream;
      const AIdData: Pointer;
      const AFactory: IVectorDataFactory
    ): IVectorItemSubset;
    function Load(
      const AData: IBinaryData;
      const AIdData: Pointer;
      const AFactory: IVectorDataFactory
    ): IVectorItemSubset;
  public
    constructor Create(
      const AFactory: IVectorItemsFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  u_StreamReadOnlyByBinaryData,
  u_DoublePointsAggregator,
  u_GeoFun,
  u_GeoToStr;

constructor TPLTSimpleParser.Create(
  const AFactory: IVectorItemsFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FFactory := AFactory;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FLoadStreamCounter := APerfCounterList.CreateAndAddNewCounter('LoadPltStream');
end;

function TPLTSimpleParser.Load(
  const AData: IBinaryData;
  const AIdData: Pointer;
  const AFactory: IVectorDataFactory
): IVectorItemSubset;
var
  VStream: TStreamReadOnlyByBinaryData;
begin
  Result := nil;
  VStream := TStreamReadOnlyByBinaryData.Create(AData);
  try
    Result := LoadFromStream(VStream, AIdData, AFactory);
  finally
    VStream.Free;
  end;
end;

function TPLTSimpleParser.LoadFromStream(
  AStream: TStream;
  const AIdData: Pointer;
  const AFactory: IVectorDataFactory
): IVectorItemSubset;
var
  pltstr: TStringList;
  trackname: string;
  VList: IVectorItemSubsetBuilder;
  VItem: IVectorDataItemSimple;
  VPointsAggregator: IDoublePointsAggregator;
begin
  Result := nil;
  pltstr := TStringList.Create;
  try
    pltstr.LoadFromStream(AStream);
    if pltstr.Count > 7 then begin
      VPointsAggregator := TDoublePointsAggregator.Create;
      ParseStringList(pltstr, VPointsAggregator);
      if VPointsAggregator.Count > 0 then begin
        trackname := GetWord(pltstr[4], ',', 4);
        VItem :=
          AFactory.BuildPath(
            AIdData,
            trackname,
            '',
            FFactory.CreateLonLatPath(VPointsAggregator.Points, VPointsAggregator.Count)
          );
        VList := FVectorItemSubsetBuilderFactory.Build;
        VList.Add(VItem);
        Result := VList.MakeStaticAndClear;
      end;
    end;
  finally
    pltstr.Free;
  end;
end;

procedure TPLTSimpleParser.ParseStringList(
  AStringList: TStringList;
  const APointsAggregator: IDoublePointsAggregator
);
var
  i, j: integer;
  VStr: string;
  VPoint: TDoublePoint;
  VValidPoint: Boolean;
begin
  for i := 6 to AStringList.Count - 1 do begin
    try
      j := 1;
      VStr := AStringList[i];
      while j < length(VStr) do begin
        if VStr[j] = ' ' then begin
          delete(VStr, j, 1);
        end else begin
          inc(j);
        end;
      end;
      if (GetWord(AStringList[i], ',', 3) = '1') and (i > 6) then begin
        VPoint := CEmptyDoublePoint;
        APointsAggregator.Add(VPoint);
      end;
      VValidPoint := True;
      try
        VPoint.y := str2r(GetWord(VStr, ',', 1));
        VPoint.x := str2r(GetWord(VStr, ',', 2));
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

function TPLTSimpleParser.GetWord(
  Str: string;
  const Smb: string;
  WordNmbr: Byte
): string;
var
  SWord: string;
  StrLen, N: Byte;
begin
  StrLen := Length(Str);
  N := 1;
  while ((WordNmbr >= N) and (StrLen <> 0)) do begin
    StrLen := System.Pos(Smb, str);
    if StrLen <> 0 then begin
      SWord := Copy(Str, 1, StrLen - 1);
      Delete(Str, 1, StrLen);
      Inc(N);
    end else begin
      SWord := Str;
    end;
  end;
  if WordNmbr <= N then begin
    Result := SWord;
  end else begin
    Result := '';
  end;
end;

end.
