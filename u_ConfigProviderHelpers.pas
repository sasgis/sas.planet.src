unit u_ConfigProviderHelpers;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider;

procedure WriteColor32(
  AConfigProvider: IConfigDataWriteProvider;
  AIdent: string;
  AValue: TColor32
);
function ReadColor32(
  AConfigProvider: IConfigDataProvider;
  AIdent: string;
  ADefault: TColor32
): TColor32;

implementation

uses
  SysUtils,
  Graphics;

function ReadColor32(
  AConfigProvider: IConfigDataProvider;
  AIdent: string;
  ADefault: TColor32
): TColor32;
var
  VColor: TColor;
  VAlfa: Integer;
  VHexString: string;
  VIntColor: Integer;
begin
  Result := ADefault;
  if AConfigProvider <> nil then begin
    VHexString := AConfigProvider.ReadString(AIdent + 'Hex', '');
    if VHexString = '' then begin
      VAlfa := AlphaComponent(Result);
      VColor := WinColor(Result);
      VAlfa := AConfigProvider.ReadInteger(AIdent + 'Alfa', VAlfa);
      VColor := AConfigProvider.ReadInteger(AIdent, VColor);
      Result := SetAlpha(Color32(VColor), VAlfa);
    end else begin
      if TryStrToInt(VHexString, VIntColor) then begin
        Result := VIntColor;
      end;
    end;
  end;
end;

procedure WriteColor32(
  AConfigProvider: IConfigDataWriteProvider;
  AIdent: string;
  AValue: TColor32
);
begin
  AConfigProvider.WriteString(AIdent + 'Hex', HexDisplayPrefix + IntToHex(AValue, 8));
end;

end.
