unit u_KmzInfoSimpleParser;

interface

uses
  Classes,
  u_KmlInfoSimple,
  u_KmlInfoSimpleParser;

type
  TKmzInfoSimpleParser = class(TKmlInfoSimpleParser)
  public
    procedure LoadFromStream(AStream: TStream; ABtm: TKmlInfoSimple); override;
  end;

implementation

uses
  SysUtils,
  KAZip;

{ TKmzInfoSimpleParser }

procedure TKmzInfoSimpleParser.LoadFromStream(AStream: TStream;
  ABtm: TKmlInfoSimple);
var
  UnZip: TKAZip;
  VMemStream: TMemoryStream;
  VStreamKml: TMemoryStream;
begin
  UnZip := TKAZip.Create(nil);
  try
    VMemStream := TMemoryStream.Create;
    try
      VMemStream.LoadFromStream(AStream);
      UnZip.Open(VMemStream);
      VStreamKml := TMemoryStream.Create;
      try
        UnZip.Entries.Items[0].ExtractToStream(VStreamKml);
        inherited LoadFromStream(VStreamKml, ABtm);
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
