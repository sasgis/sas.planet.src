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
    procedure ParseSubstList(const ASubstListText: string);
    procedure ParseSubstListItem(const ASubstListItemText: string);
  protected
    function GetContentType(const ASource: string): string;
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
  FSource.Sorted := True;
end;

destructor TContentTypeSubstByList.Destroy;
begin
  FreeAndNil(FSource);
  FreeAndNil(FTarget);
  inherited;
end;

function TContentTypeSubstByList.GetContentType(const ASource: string): string;
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

procedure TContentTypeSubstByList.ParseSubstList(const ASubstListText: string);
var
  VTempList: TStringList;
  i: Integer;
  VSubstItemText: string;
begin
  VTempList := TStringList.Create;
  try
    VTempList.QuoteChar := '"';
    VTempList.Delimiter := ';';
    VTempList.DelimitedText := ASubstListText;
    for i := 0 to VTempList.Count - 1 do begin
      VSubstItemText := VTempList.Strings[i];
      if VSubstItemText <> '' then begin
        ParseSubstListItem(VSubstItemText);
      end;
    end;
  finally
    VTempList.Free;
  end;
end;

procedure TContentTypeSubstByList.ParseSubstListItem(
  const ASubstListItemText: string);
var
  VTempList: TStringList;
  VSource: string;
  VTarget: string;
  VTargetIndex: Integer;
begin
  VTempList := TStringList.Create;
  try
    VTempList.QuoteChar := '"';
    VTempList.Delimiter := '=';
    VTempList.DelimitedText := ASubstListItemText;
    if VTempList.Count = 2 then begin
      VSource := VTempList.Strings[0];
      VTarget := VTempList.Strings[1];
      if (VSource <> '') and (VTarget <> '') then begin
        VTargetIndex := FTarget.Add(VTarget);
        FSource.AddObject(VSource, TObject(VTargetIndex));
      end; 
    end;
  finally
    VTempList.Free;
  end;
end;

end.
