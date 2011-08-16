unit u_KmzInfoSimpleParser;

interface

uses
  Classes,
  i_VectorDataItemSimple,
  i_InternalPerformanceCounter,
  i_HtmlToHintTextConverter,
  u_KmlInfoSimpleParser;

type
  TKmzInfoSimpleParser = class(TKmlInfoSimpleParser)
  private
    FLoadKmzStreamCounter: IInternalPerformanceCounter;
  protected
    procedure LoadFromStream(AStream: TStream; out AItems: IVectorDataItemList); override;
  public
    constructor Create(
      AHintConverter: IHtmlToHintTextConverter;
      APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  SysUtils,
  KAZip;

{ TKmzInfoSimpleParser }

constructor TKmzInfoSimpleParser.Create(
  AHintConverter: IHtmlToHintTextConverter;
  APerfCounterList: IInternalPerformanceCounterList);
var
  VPerfCounterList: IInternalPerformanceCounterList;
begin
  VPerfCounterList := APerfCounterList.CreateAndAddNewSubList('KmzLoader');
  inherited Create(AHintConverter, VPerfCounterList);
  FLoadKmzStreamCounter := VPerfCounterList.CreateAndAddNewCounter('LoadKmzStream');
end;

procedure TKmzInfoSimpleParser.LoadFromStream(AStream: TStream;
  out AItems: IVectorDataItemList);
var
  i: Integer;
  UnZip: TKAZip;
  VMemStream: TMemoryStream;
  VStreamKml: TMemoryStream;
  VIndex: Integer;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FLoadKmzStreamCounter.StartOperation;
  try
    UnZip := TKAZip.Create(nil);
    try
      VMemStream := TMemoryStream.Create;
      try
        VMemStream.LoadFromStream(AStream);
        UnZip.Open(VMemStream);
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
          inherited LoadFromStream(VStreamKml, AItems);
        finally
          VStreamKml.Free;
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
