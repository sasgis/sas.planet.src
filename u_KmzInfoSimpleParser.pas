unit u_KmzInfoSimpleParser;

interface

uses
  Classes,
  i_VectorDataItemSimple,
  u_KmlInfoSimpleParser;

type
  TKmzInfoSimpleParser = class(TKmlInfoSimpleParser)
  public
    procedure LoadFromStream(AStream: TStream; out AItems: IVectorDataItemList); override;
  end;

implementation

uses
  SysUtils,
  KAZip;

{ TKmzInfoSimpleParser }

procedure TKmzInfoSimpleParser.LoadFromStream(AStream: TStream;
  out AItems: IVectorDataItemList);
var
  UnZip: TKAZip;
  VMemStream: TMemoryStream;
  VStreamKml: TMemoryStream;
  VIndex: Integer;
begin
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
end;

end.
