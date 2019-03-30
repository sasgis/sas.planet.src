unit u_AppearanceHelper;

interface

uses
  i_Appearance,
  i_AppearanceOfMarkFactory,
  i_AppearanceHelper,
  i_MarkPicture,
  i_ImportConfig,
  u_BaseInterfacedObject;

type
  TAppearanceHelper = class(TBaseInterfacedObject, IAppearanceHelper)
  private
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;

    FPointParams: IImportPointParams;
    FLineParams: IImportLineParams;
    FPolygonParams: IImportPolyParams;

    FTextColor: IColorSetHelper;
    FLineColor: IColorSetHelper;
    FFillColor: IColorSetHelper;
    FLineWidth: IIntegerSetHelper;
    FTextSize: IIntegerSetHelper;
    FIconSize: IIntegerSetHelper;
    FIcon: IIconSetHelper;
  private
    { IAppearanceHelper }

    procedure Reset;

    function GetTextColor: IColorSetHelper;
    function GetLineColor: IColorSetHelper;
    function GetFillColor: IColorSetHelper;
    function GetLineWidth: IIntegerSetHelper;
    function GetTextSize: IIntegerSetHelper;
    function GetIconSize: IIntegerSetHelper;
    function GetIcon: IIconSetHelper;

    function GetHasPointAppearance: Boolean;
    function GetHasLineAppearance: Boolean;
    function GetHasPolygonAppearance: Boolean;

    function RedefinePointAppearance: IAppearance;
    function RedefineLineAppearance: IAppearance;
    function RedefinePolygonAppearance: IAppearance;
  public
    constructor Create(
      const APointParams: IImportPointParams;
      const ALineParams: IImportLineParams;
      const APolygonParams: IImportPolyParams;
      const AMarkPictureList: IMarkPictureList;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory
    );
  end;

implementation

uses
  SysUtils,
  t_Bitmap32,
  c_Color32;

type
  TColorSetHelper = class(TBaseInterfacedObject, IColorSetHelper)
  private
    FColor: TColor32;
    FFound: Boolean;
  private
    procedure Reset;

    function GetColor: TColor32;
    procedure SetColor(AValue: TColor32);

    function GetFound: Boolean;

    function SetGPXColorName(const AName: WideString): Boolean;
    procedure SetKMLColorValue(const AValue: LongWord);
  public
    constructor Create;
  end;

{ TColorSetHelper }

constructor TColorSetHelper.Create;
begin
  inherited Create;
  FColor := 0;
  FFound := False;
end;

function TColorSetHelper.GetColor: TColor32;
begin
  Result := FColor;
end;

function TColorSetHelper.GetFound: Boolean;
begin
  Result := FFound;
end;

procedure TColorSetHelper.Reset;
begin
  FColor := 0;
  FFound := False;
end;

procedure TColorSetHelper.SetColor(AValue: TColor32);
begin
  FColor := AValue;
  FFound := True;
end;

function TColorSetHelper.SetGPXColorName(const AName: WideString): Boolean;
begin
  FFound := False;
  FColor := clBlack32;

  case Length(AName) of
    3: begin
      // Red
      if WideSameText(AName, 'Red') then begin
        FColor := clRed32;
        FFound := True;
      end;
    end;
    4: begin
      // Blue
      // Cyan
      if WideSameText(AName, 'Blue') then begin
        FColor := clBlue32;
        FFound := True;
      end else if WideSameText(AName, 'Cyan') then begin
        FColor := clAqua32;
        FFound := True;
      end;
    end;
    5: begin
      // Black
      // Green
      // White
      if WideSameText(AName, 'Black') then begin
        FColor := clBlack32;
        FFound := True;
      end else if WideSameText(AName, 'Green') then begin
        FColor := clGreen32;
        FFound := True;
      end else if WideSameText(AName, 'White') then begin
        FColor := clWhite32;
        FFound := True;
      end;
    end;
    6: begin
      // Yellow
      if WideSameText(AName, 'Yellow') then begin
        FColor := clLightYellow32;
        FFound := True;
      end;
    end;
    7: begin
      // DarkRed
      // Magenta
      if WideSameText(AName, 'DarkRed') then begin
        FColor := clDarkRed32;
        FFound := True;
      end else if WideSameText(AName, 'Magenta') then begin
        FColor := clDarkMagenta32;
        FFound := True;
      end;
    end;
    8: begin
      // DarkBlue
      // DarkCyan,
      // DarkGray
      if WideSameText(AName, 'DarkBlue') then begin
        FColor := clDarkBlue32;
        FFound := True;
      end else if WideSameText(AName, 'DarkCyan') then begin
        FColor := clDarkCyan32;
        FFound := True;
      end else if WideSameText(AName, 'DarkGray') then begin
        FColor := clDarkGray32;
        FFound := True;
      end;
    end;
    9: begin
      // DarkGreen
      // LightGray
      if WideSameText(AName, 'DarkGreen') then begin
        FColor := clDarkGreen32;
        FFound := True;
      end else if WideSameText(AName, 'LightGray') then begin
        FColor := clLightGray32;
        FFound := True;
      end;
    end;
    10: begin
      // DarkYellow
      if WideSameText(AName, 'DarkYellow') then begin
        FColor := clYellow32;
        FFound := True;
      end;
    end;
    11: begin
      // DarkMagenta
      // Transparent
      if WideSameText(AName, 'DarkMagenta') then begin
        FColor := clDarkMagenta32;
        FFound := True;
      end else if WideSameText(AName, 'Transparent') then begin
        FColor := $00000000;
        FFound := True;
      end;
    end;
  end;

  Result := FFound;
end;

procedure TColorSetHelper.SetKMLColorValue(const AValue: LongWord);
begin
  FColor := TColor32(AValue);
  TColor32Entry(FColor).B := TColor32Entry(AValue).R;
  TColor32Entry(FColor).R := TColor32Entry(AValue).B;
  FFound := True;
end;

type
  TIntegerSetHelper = class(TBaseInterfacedObject, IIntegerSetHelper)
  private
    FValue: Integer;
    FFound: Boolean;
  private
    procedure Reset;

    function GetValue: Integer;
    procedure SetValue(AValue: Integer);

    function GetFound: Boolean;
  public
    constructor Create;
  end;

{ TIntegerSetHelper }

constructor TIntegerSetHelper.Create;
begin
  inherited Create;
  FValue := 0;
  FFound := False;
end;

function TIntegerSetHelper.GetFound: Boolean;
begin
  Result := FFound;
end;

function TIntegerSetHelper.GetValue: Integer;
begin
  Result := FValue;
end;

procedure TIntegerSetHelper.Reset;
begin
  FValue := 0;
  FFound := False;
end;

procedure TIntegerSetHelper.SetValue(AValue: Integer);
begin
  FValue := AValue;
  FFound := True;
end;

type
  TIconSetHelper = class(TBaseInterfacedObject, IIconSetHelper)
  private
    FMarkPictureList: IMarkPictureList;

    FIcon: IMarkPicture;
    FFound: Boolean;
  private
    procedure Reset;

    function GetIcon: IMarkPicture;
    procedure SetIcon(const AIcon: IMarkPicture);

    function GetFound: Boolean;

    function SetByName(const AName: String): Boolean;
  public
    constructor Create(const AMarkPictureList: IMarkPictureList);
  end;

{ TIconSetHelper }

constructor TIconSetHelper.Create(const AMarkPictureList: IMarkPictureList);
begin
  inherited Create;
  FMarkPictureList := AMarkPictureList;

  FIcon := nil;
  FFound := False;
end;

function TIconSetHelper.GetFound: Boolean;
begin
  Result := FFound;
end;

function TIconSetHelper.GetIcon: IMarkPicture;
begin
  Result := FIcon;
end;

procedure TIconSetHelper.Reset;
begin
  FIcon := nil;
  FFound := False;
end;

function TIconSetHelper.SetByName(const AName: String): Boolean;
var
  VIcon: IMarkPicture;
begin
  if Assigned(FMarkPictureList) then begin
    VIcon := FMarkPictureList.FindByName(AName);
    if Assigned(VIcon) then begin
      FFound := True;
      FIcon := VIcon;
      Result := True;
    end else begin
      Result := False;
    end;
  end else begin
    Result := False;
  end;
end;

procedure TIconSetHelper.SetIcon(const AIcon: IMarkPicture);
begin
  FIcon := AIcon;
  FFound := True;
end;

{ TAppearanceHelper }

constructor TAppearanceHelper.Create(
  const APointParams: IImportPointParams;
  const ALineParams: IImportLineParams;
  const APolygonParams: IImportPolyParams;
  const AMarkPictureList: IMarkPictureList;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory
);
begin
  Assert(Assigned(AAppearanceOfMarkFactory));
  inherited Create;
  FPointParams := APointParams;
  FLineParams := ALineParams;
  FPolygonParams := APolygonParams;
  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;


  FTextColor := TColorSetHelper.Create;
  FLineColor := TColorSetHelper.Create;
  FFillColor := TColorSetHelper.Create;
  FLineWidth := TIntegerSetHelper.Create;
  FTextSize := TIntegerSetHelper.Create;
  FIconSize := TIntegerSetHelper.Create;
  FIcon := TIconSetHelper.Create(AMarkPictureList);
end;

function TAppearanceHelper.RedefinePointAppearance: IAppearance;
var
  VTextColor: TColor32;
  VFontSize: Integer;
  VTextBgColor: TColor32;
  VMarkerSize: Integer;
  VPic: IMarkPicture;
  VPicName: string;
begin
  if Assigned(FPointParams) then begin
    if not FPointParams.IsForcePicName and FIcon.Found then begin
      VPic := FIcon.Icon;
      if Assigned(VPic) then begin
        VPicName := VPic.GetName;
      end else begin
        VPicName := '';
      end;
    end else begin
      VPic := FPointParams.IconAppearance.Pic;
      VPicName := FPointParams.IconAppearance.PicName;
    end;

    // fill color
    if not FPointParams.IsForceTextColor and FTextColor.Found then
      VTextColor := FTextColor.Color
    else
      VTextColor := FPointParams.CaptionAppearance.TextColor;

    // back color
    if not FPointParams.IsForceTextBgColor and FFillColor.Found then
      VTextBgColor := FFillColor.Color
    else
      VTextBgColor := FPointParams.CaptionAppearance.TextBgColor;

    // font size
    if not FPointParams.IsForceFontSize and FTextSize.Found then
      VFontSize := FTextSize.Value
    else
      VFontSize := FPointParams.CaptionAppearance.FontSize;

    // marker size
    if not FPointParams.IsForceMarkerSize and FIconSize.Found then
      VMarkerSize := FIconSize.Value
    else
      VMarkerSize := FPointParams.IconAppearance.MarkerSize;

    Result := FAppearanceOfMarkFactory.CreatePointAppearance(
      VTextColor,
      VTextBgColor,
      VFontSize,
      VPicName,
      VPic,
      VMarkerSize
    );
  end else begin
    Result := nil;
  end;
end;

function TAppearanceHelper.RedefineLineAppearance: IAppearance;
var
  VLineColor: TColor32; // Color1
  VLineWidth: Integer;  // Scale1
begin
  if Assigned(FLineParams) then begin
    // line color
    if not FLineParams.IsForceLineColor and FLineColor.Found then
      VLineColor := FLineColor.Color
    else
      VLineColor := FLineParams.LineAppearance.LineColor;

    // line width
    if not FLineParams.IsForceLineWidth and FLineWidth.Found then
      VLineWidth := FLineWidth.Value
    else
      VLineWidth := FLineParams.LineAppearance.LineWidth;

    Result := FAppearanceOfMarkFactory.CreateLineAppearance(
      VLineColor,
      VLineWidth
    );
  end else begin
    Result := nil;
  end;
end;

function TAppearanceHelper.RedefinePolygonAppearance: IAppearance;
var
  VLineColor: TColor32; // Color1
  VLineWidth: Integer;  // Scale1
  VFillColor: TColor32; // Color2
begin
  if Assigned(FPolygonParams) then begin
    // border color
    if not FPolygonParams.IsForceLineColor and FLineColor.Found then
      VLineColor := FLineColor.Color
    else
      VLineColor := FPolygonParams.BorderAppearance.LineColor;

    // line width
    if not FPolygonParams.IsForceLineWidth and FLineWidth.Found then
      VLineWidth := FLineWidth.Value
    else
      VLineWidth := FPolygonParams.BorderAppearance.LineWidth;

    // fill color
    if not FPolygonParams.IsForceFillColor and FFillColor.Found then
      VFillColor := FFillColor.Color
    else
      VFillColor := FPolygonParams.FillAppearance.FillColor;

    Result :=
      FAppearanceOfMarkFactory.CreatePolygonAppearance(
        VLineColor,
        VLineWidth,
        VFillColor
      );
  end else begin
    Result := nil;
  end;
end;

function TAppearanceHelper.GetFillColor: IColorSetHelper;
begin
  Result := FFillColor;
end;

function TAppearanceHelper.GetHasPointAppearance: Boolean;
begin
  Result :=
    FTextColor.Found or
    FFillColor.Found or
    FTextSize.Found or
    FIconSize.Found or
    FIcon.Found;
end;

function TAppearanceHelper.GetHasLineAppearance: Boolean;
begin
  Result :=
    FLineColor.Found or
    FLineWidth.Found;
end;

function TAppearanceHelper.GetHasPolygonAppearance: Boolean;
begin
  Result :=
    FLineColor.Found or
    FFillColor.Found or
    FLineWidth.Found;
end;

function TAppearanceHelper.GetIcon: IIconSetHelper;
begin
  Result := FIcon;
end;

function TAppearanceHelper.GetIconSize: IIntegerSetHelper;
begin
  Result := FIconSize;
end;

function TAppearanceHelper.GetLineColor: IColorSetHelper;
begin
  Result := FLineColor;
end;

function TAppearanceHelper.GetLineWidth: IIntegerSetHelper;
begin
  Result := FLineWidth;
end;

function TAppearanceHelper.GetTextColor: IColorSetHelper;
begin
  Result := FTextColor;
end;

function TAppearanceHelper.GetTextSize: IIntegerSetHelper;
begin
  Result := FTextSize;
end;

procedure TAppearanceHelper.Reset;
begin
  FTextColor.Reset;
  FLineColor.Reset;
  FFillColor.Reset;
  FLineWidth.Reset;
  FTextSize.Reset;
  FIconSize.Reset;
  FIcon.Reset;
end;

end.
