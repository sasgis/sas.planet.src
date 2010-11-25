unit u_ConfigProviderHelpers;

interface

uses
  GR32,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider;

procedure WriteColor32(
  AConfigProvider: IConfigDataWriteProvider;
  AIdent: string;
  AValue: TColor32
);
function LoadColor32(
  AConfigProvider: IConfigDataProvider;
  AIdent: string;
  ADefault: TColor32
): TColor32;

implementation

uses
  Graphics;
  
function LoadColor32(
  AConfigProvider: IConfigDataProvider;
  AIdent: string;
  ADefault: TColor32
): TColor32;
var
  VColor: TColor;
  VAlfa: Integer;
begin
  Result := ADefault;
  if AConfigProvider <> nil then begin
    VAlfa := AlphaComponent(Result);
    VColor := WinColor(Result);
    VAlfa := AConfigProvider.ReadInteger(AIdent + 'Alfa', VAlfa);
    VColor := AConfigProvider.ReadInteger(AIdent, VColor);
    Result := SetAlpha(Color32(VColor), VAlfa);
  end;
end;

procedure WriteColor32(
  AConfigProvider: IConfigDataWriteProvider;
  AIdent: string;
  AValue: TColor32
);
begin
  AConfigProvider.WriteInteger(AIdent + 'Alfa', AlphaComponent(AValue));
  AConfigProvider.WriteInteger(AIdent, WinColor(AValue));
end;

end.
