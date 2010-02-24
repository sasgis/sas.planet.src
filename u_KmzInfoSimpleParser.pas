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
  VCLUnZip;

{ TKmzInfoSimpleParser }

procedure TKmzInfoSimpleParser.LoadFromStream(AStream: TStream;
  ABtm: TKmlInfoSimple);
var
  UnZip:TVCLUnZip;
  VMemStream: TMemoryStream;
  VStreamKml: TMemoryStream;
begin
  UnZip:=TVCLUnZip.Create(nil);
  try
    VMemStream := TMemoryStream.Create;
    try
      UnZip.ArchiveStream:= VMemStream;
      VMemStream.LoadFromStream(AStream);
      UnZip.ReadZip;
      VStreamKml := TMemoryStream.Create;
      try
        UnZip.UnZipToStream(VStreamKml,UnZip.Filename[0]);
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
