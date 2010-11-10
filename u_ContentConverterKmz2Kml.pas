unit u_ContentConverterKmz2Kml;

interface

uses
  Classes,
  i_ContentTypeInfo,
  u_ContentConverterBase;

type
  TContentConverterKmz2Kml = class(TContentConverterBase)
  protected
    procedure ConvertStream(ASource, ATarget: TStream); override;
  end;

implementation

uses
  KAZip;

{ TContentConverterKmz2Kml }

procedure TContentConverterKmz2Kml.ConvertStream(ASource, ATarget: TStream);
var
  UnZip:TKAZip;
begin
  inherited;
  UnZip:=TKAZip.Create(nil);
  try
    UnZip.Open(ASource);
    UnZip.Entries.Items[0].ExtractToStream(ATarget);
  finally
    UnZip.Free;
  end;
end;

end.
