unit u_ContentTypeSubstByList;

interface

uses
  Classes,
  i_ConfigDataProvider,
  i_ContentTypeSubst;

type
  TContentTypeSubstByList = class(TInterfacedObject, IContentTypeSubst)
  private
    FSource: TStringList;
    FTarget: TStringList;
    procedure ParseSubstList(ASubstListText: string);
  protected
    function GetContentType(ASource: string): string;
  public
    constructor Create(AConfig: IConfigDataProvider);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TContentTypeSubstByList }

constructor TContentTypeSubstByList.Create(AConfig: IConfigDataProvider);
var
  VSubstListText: string;
begin
  FSource := TStringList.Create;
  FTarget := TStringList.Create;
  VSubstListText := AConfig.ReadString('MimeTypeSubst', '');
  if VSubstListText <> '' then begin
    ParseSubstList(VSubstListText);
  end;
end;

destructor TContentTypeSubstByList.Destroy;
begin
  FreeAndNil(FSource);
  FreeAndNil(FTarget);
  inherited;
end;

function TContentTypeSubstByList.GetContentType(ASource: string): string;
var
  VSourceIndex: Integer;
  VTargetIndex: Integer;
begin
  if FSource.Find(ASource, VSourceIndex) then begin
    VTargetIndex := Integer(FSource.Objects[VSourceIndex]);
    Result := FTarget.Strings[VTargetIndex];
  end else begin
    Result := ASource;
  end;
end;

procedure TContentTypeSubstByList.ParseSubstList(ASubstListText: string);
var
  VTempList: TStringList;
  i: Integer;
  VSubstItemString: string;
begin
  VTempList := TStringList.Create;
  try
    VTempList.QuoteChar := '"';
    VTempList.Delimiter := ';';
    VTempList.DelimitedText := ASubstListText;
    for i := 0 to VTempList.Count - 1 do begin
      VSubstItemString := VTempList.Strings[i];
    end;
  finally
    VTempList.Free;
  end;
end;

end.
