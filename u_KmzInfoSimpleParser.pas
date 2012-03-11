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

unit u_KmzInfoSimpleParser;

interface

uses
  Classes,
  i_BinaryData,
  i_VectorDataFactory,
  i_VectorDataItemSimple,
  i_VectorItmesFactory,
  i_InternalPerformanceCounter,
  i_HtmlToHintTextConverter,
  u_KmlInfoSimpleParser;

type
  TKmzInfoSimpleParser = class(TKmlInfoSimpleParser)
  private
    FLoadKmzStreamCounter: IInternalPerformanceCounter;
  protected
    function LoadFromStream(AStream: TStream; AFactory: IVectorDataFactory): IVectorDataItemList; override;
    function Load(AData: IBinaryData; AFactory: IVectorDataFactory): IVectorDataItemList; override;
  public
    constructor Create(
      AFactory: IVectorItmesFactory;
      APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  SysUtils,
  KAZip,
  u_StreamReadOnlyByBinaryData;

{ TKmzInfoSimpleParser }

constructor TKmzInfoSimpleParser.Create(
  AFactory: IVectorItmesFactory;
  APerfCounterList: IInternalPerformanceCounterList);
var
  VPerfCounterList: IInternalPerformanceCounterList;
begin
  VPerfCounterList := APerfCounterList.CreateAndAddNewSubList('KmzLoader');
  inherited Create(AFactory, VPerfCounterList);
  FLoadKmzStreamCounter := VPerfCounterList.CreateAndAddNewCounter('LoadKmzStream');
end;

function TKmzInfoSimpleParser.Load(AData: IBinaryData; AFactory: IVectorDataFactory): IVectorDataItemList;
var
  VStream: TStreamReadOnlyByBinaryData;
begin
  Result := nil;
  VStream := TStreamReadOnlyByBinaryData.Create(AData);
  try
    Result := LoadFromStream(VStream, AFactory);
  finally
    VStream.Free;
  end;
end;

function TKmzInfoSimpleParser.LoadFromStream(AStream: TStream; AFactory: IVectorDataFactory): IVectorDataItemList;
var
  i: Integer;
  UnZip: TKAZip;
  VMemStream: TMemoryStream;
  VStreamKml: TMemoryStream;
  VIndex: Integer;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  Result := nil;
  VCounterContext := FLoadKmzStreamCounter.StartOperation;
  try
    UnZip := TKAZip.Create(nil);
    try
      VMemStream := TMemoryStream.Create;
      try
        VMemStream.LoadFromStream(AStream);
        UnZip.Open(VMemStream);
        if UnZip.Entries.Count > 0 then begin
          VStreamKml := TMemoryStream.Create;
          try
            VIndex := UnZip.Entries.IndexOf('doc.kml');
            if VIndex < 0 then begin
              for i := 0 to UnZip.Entries.Count - 1 do begin
                if ExtractFileExt(UnZip.Entries.Items[i].FileName) =  '.kml' then begin
                  VIndex := i;
                  Break;
                end;
              end;
            end;
            if VIndex < 0 then begin
              VIndex := 0;
            end;
            UnZip.Entries.Items[VIndex].ExtractToStream(VStreamKml);
            Result := inherited LoadFromStream(VStreamKml, AFactory);
          finally
            VStreamKml.Free;
          end;
        end;
      finally
        FreeAndNil(VMemStream);
      end;
    finally
      UnZip.Free;
    end;
  finally
    FLoadKmzStreamCounter.FinishOperation(VCounterContext);
  end;
end;

end.
