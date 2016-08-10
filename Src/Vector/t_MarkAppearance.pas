unit t_MarkAppearance;

interface

uses
  Windows,
  GR32;

type
  PPointAppearanceData = ^TPointAppearanceData;
  TPointAppearanceData = record
  public
    FTextColor: TColor32;   // Color1
    FFontSize: Integer;     // Scale1
    FTextBgColor: TColor32; // Color2
    FMarkerSize: Integer;   // Scale2
  public
    function IsEqual(const ARec: PPointAppearanceData): Boolean;
    function IsSet: Boolean;
  end;

  PPolylineAppearanceData = ^TPolylineAppearanceData;
  TPolylineAppearanceData = record
  public
    FLineColor: TColor32; // Color1
    FLineWidth: Integer;  // Scale1
  public
    function IsEqual(const ARec: PPolylineAppearanceData): Boolean;
    function IsSet: Boolean;
  end;

  PPolygonAppearanceData = ^TPolygonAppearanceData;
  TPolygonAppearanceData = record
  public
    FLineAppearance: TPolylineAppearanceData;
    FFillColor: TColor32; // Color2
  public
    function IsEqual(const ARec: PPolygonAppearanceData): Boolean;
    function IsSet: Boolean;
  end;

  PAnyMarkAppearanceData = ^TAnyMarkAppearanceData;
  TAnyMarkAppearanceData = record
  case Byte of
  0: (PointData: TPointAppearanceData);
  1: (PolylineData: TPolylineAppearanceData);
  2: (PolygonData: TPolygonAppearanceData);
  3: (
    // raw
    FColor1: TColor32;
    FScale1: Integer;
    FColor2: TColor32;
    FScale2: Integer;
  );
  end;

  TColor32Definition = record
    FColor: TColor32;
    FFound: Boolean;
    function SetGPXColorName(const AName: WideString): Boolean;
    procedure SetKMLColorValue(const AValue: DWORD);
  end;

  PAppearanceRedefinitions = ^TAppearanceRedefinitions;
  TAppearanceRedefinitions = record
    FTextColorDef: TColor32Definition;
    FLineColorDef: TColor32Definition;
    FFillColorDef: TColor32Definition;
    FLineWidth: Integer; // 0 for undefined
    FTextSize: Integer;  // 0 for undefined
    FIconSize: Integer;  // 0 for undefined (based on 64 if image not found)
    procedure Init;
  strict private
    function GetHasSomeValues: Boolean;
  public
    property HasSomeValues: Boolean read GetHasSomeValues;
  end;

const
  CPointAppearanceData: TPointAppearanceData = (
    FTextColor: $FF7F0000;
    FFontSize: 12;
    FTextBgColor: $7FFFFFFF;
    FMarkerSize: 32
  );

  CPolylineAppearanceData: TPolylineAppearanceData = (
    FLineColor: $FFFF0000;
    FLineWidth: 2
  );

  CPolygonAppearanceData: TPolygonAppearanceData = (
    FLineAppearance: (FLineColor: $FFFF00FF; FLineWidth: 2);
    FFillColor: 0
  );

function Color32toKMLColor(const AColor32: TColor32): string;

implementation

uses
  SysUtils;

function Color32toKMLColor(const AColor32: TColor32): string;
begin
  result := IntToHex(AlphaComponent(AColor32), 2) +
    IntToHex(BlueComponent(AColor32), 2) +
    IntToHex(GreenComponent(AColor32), 2) +
    IntToHex(RedComponent(AColor32), 2);
end;

{ TPointAppearanceData }

function TPointAppearanceData.IsEqual(
  const ARec: PPointAppearanceData): Boolean;
begin
  Result := (Self.FTextColor = ARec^.FTextColor) and
            (Self.FTextBgColor = ARec^.FTextBgColor) and
            (Self.FFontSize = ARec^.FFontSize) and
            (Self.FMarkerSize = ARec^.FMarkerSize);
end;

function TPointAppearanceData.IsSet: Boolean;
begin
  Result := (FFontSize <> 0) or (FMarkerSize <> 0);
end;

{ TPolylineAppearanceData }

function TPolylineAppearanceData.IsEqual(
  const ARec: PPolylineAppearanceData): Boolean;
begin
  Result := (Self.FLineColor = ARec^.FLineColor) and
            (Self.FLineWidth = ARec^.FLineWidth);
end;

function TPolylineAppearanceData.IsSet: Boolean;
begin
  Result := (FLineWidth <> 0);
end;

{ TPolygonAppearanceData }

function TPolygonAppearanceData.IsEqual(
  const ARec: PPolygonAppearanceData): Boolean;
begin
  Result := (Self.FFillColor = ARec^.FFillColor) and
            (Self.FLineAppearance.IsEqual(@ARec^.FLineAppearance));
end;

function TPolygonAppearanceData.IsSet: Boolean;
begin
  Result := FLineAppearance.IsSet;
end;

{ TColor32Definition }

function TColor32Definition.SetGPXColorName(const AName: WideString): Boolean;
begin
  FFound := False;
  FColor := clBlack32;

  case Length(AName) of
    3: begin
      // Red
      if WideSameText(AName, 'Red') then begin
        Inc(FFound);
        FColor := clRed32;
      end;
    end;
    4: begin
      // Blue
      // Cyan
      if WideSameText(AName, 'Blue') then begin
        Inc(FFound);
        FColor := clBlue32;
      end else if WideSameText(AName, 'Cyan') then begin
        Inc(FFound);
        FColor := clAqua32;
      end;
    end;
    5: begin
      // Black
      // Green
      // White
      if WideSameText(AName, 'Black') then begin
        Inc(FFound);
        FColor := clBlack32;
      end else if WideSameText(AName, 'Green') then begin
        Inc(FFound);
        FColor := clGreen32;
      end else if WideSameText(AName, 'White') then begin
        Inc(FFound);
        FColor := clWhite32;
      end;
    end;
    6: begin
      // Yellow
      if WideSameText(AName, 'Yellow') then begin
        Inc(FFound);
        FColor := clLightYellow32;
      end;
    end;
    7: begin
      // DarkRed
      // Magenta
      if WideSameText(AName, 'DarkRed') then begin
        Inc(FFound);
        FColor := clDarkRed32;
      end else if WideSameText(AName, 'Magenta') then begin
        Inc(FFound);
        FColor := clDarkMagenta32;
      end;
    end;
    8: begin
      // DarkBlue
      // DarkCyan,
      // DarkGray
      if WideSameText(AName, 'DarkBlue') then begin
        Inc(FFound);
        FColor := clDarkBlue32;
      end else if WideSameText(AName, 'DarkCyan') then begin
        Inc(FFound);
        FColor := clDarkCyan32;
      end else if WideSameText(AName, 'DarkGray') then begin
        Inc(FFound);
        FColor := clDarkGray32;
      end;
    end;
    9: begin
      // DarkGreen
      // LightGray
      if WideSameText(AName, 'DarkGreen') then begin
        Inc(FFound);
        FColor := clDarkGreen32;
      end else if WideSameText(AName, 'LightGray') then begin
        Inc(FFound);
        FColor := clLightGray32;
      end;
    end;
    10: begin
      // DarkYellow
      if WideSameText(AName, 'DarkYellow') then begin
        Inc(FFound);
        FColor := clYellow32;
      end;
    end;
    11: begin
      // DarkMagenta
      // Transparent
      if WideSameText(AName, 'DarkMagenta') then begin
        Inc(FFound);
        FColor := clDarkMagenta32;
      end else if WideSameText(AName, 'Transparent') then begin
        Inc(FFound);
        FColor := $00000000;
      end;
    end;
  end;

  Result := FFound;
end;

procedure TColor32Definition.SetKMLColorValue(const AValue: DWORD);
begin
  FColor := TColor32(AValue);
  TColor32Entry(FColor).B := TColor32Entry(AValue).R;
  TColor32Entry(FColor).R := TColor32Entry(AValue).B;
  FFound := True;
end;

{ TAppearanceRedefinitions }

function TAppearanceRedefinitions.GetHasSomeValues: Boolean;
begin
  Result := FTextColorDef.FFound or FLineColorDef.FFound or FFillColorDef.FFound or
            (FLineWidth > 0) or (FTextSize > 0) or (FIconSize > 0);
end;

procedure TAppearanceRedefinitions.Init;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

end.
