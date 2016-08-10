unit u_AppearanceHelper;

interface

uses
  t_MarkAppearance,
  i_Appearance,
  i_AppearanceOfMarkFactory,
  i_AppearanceHelper,
  i_MarkPicture,
  i_ImportConfig,
  u_BaseInterfacedObject;

type
  TAppearanceHelper = class(TBaseInterfacedObject, IAppearanceHelper)
  private
    FMarkPictureList: IMarkPictureList;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  private
    { IAppearanceHelper }
    function GetAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    function GetMarkPictureList: IMarkPictureList;
    
    function LoadPointAppearance(
      const AAppearance: PPointAppearanceData;
      const APicName: string;
      const APicOptional: IMarkPicture
    ): IAppearance;

    function LoadPolylineAppearance(
      const AAppearance: PPolylineAppearanceData
    ): IAppearance;

    function LoadPolygonAppearance(
      const AAppearance: PPolygonAppearanceData
    ): IAppearance;

    function RedefinePointAppearance(
      const APointParams: IImportPointParams;
      const ARedefinitions: TAppearanceRedefinitions;
      const ARedefinePic: IMarkPicture
    ): IAppearance;

    function RedefineLineAppearance(
      const ALineParams: IImportLineParams;
      const ARedefinitions: TAppearanceRedefinitions
    ): IAppearance;

    function RedefinePolygonAppearance(
      const APolygonParams: IImportPolyParams;
      const ARedefinitions: TAppearanceRedefinitions
    ): IAppearance;
  public
    constructor Create(
      const AMarkPictureList: IMarkPictureList;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory
    );
  end;

implementation

{ TAppearanceHelper }

constructor TAppearanceHelper.Create(
  const AMarkPictureList: IMarkPictureList;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory
);
begin
  Assert(Assigned(AMarkPictureList));
  Assert(Assigned(AAppearanceOfMarkFactory));
  inherited Create;
  FMarkPictureList := AMarkPictureList;
  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;
end;

function TAppearanceHelper.GetAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
begin
  Result := FAppearanceOfMarkFactory;
end;

function TAppearanceHelper.GetMarkPictureList: IMarkPictureList;
begin
  Result := FMarkPictureList;
end;

function TAppearanceHelper.LoadPointAppearance(
  const AAppearance: PPointAppearanceData;
  const APicName: string;
  const APicOptional: IMarkPicture
): IAppearance;
var
  VPic: IMarkPicture;
begin
  VPic := APicOptional;
  if not Assigned(VPic) then begin
    VPic := FMarkPictureList.FindByNameOrDefault(APicName);
  end;

  Result := FAppearanceOfMarkFactory.CreatePointAppearance(
    AAppearance.FTextColor,
    AAppearance.FTextBgColor,
    AAppearance.FFontSize,
    APicName,
    VPic,
    AAppearance.FMarkerSize
  );
end;

function TAppearanceHelper.LoadPolygonAppearance(
  const AAppearance: PPolygonAppearanceData
): IAppearance;
begin
  Result := FAppearanceOfMarkFactory.CreatePolygonAppearance(
    AAppearance.FLineAppearance.FLineColor,
    AAppearance.FLineAppearance.FLineWidth,
    AAppearance.FFillColor
  );
end;

function TAppearanceHelper.LoadPolylineAppearance(
  const AAppearance: PPolylineAppearanceData
): IAppearance;
begin
  Result := FAppearanceOfMarkFactory.CreateLineAppearance(
    AAppearance.FLineColor,
    AAppearance.FLineWidth
  );
end;

function TAppearanceHelper.RedefineLineAppearance(
  const ALineParams: IImportLineParams;
  const ARedefinitions: TAppearanceRedefinitions
): IAppearance;
var
  VAppearance: TPolylineAppearanceData;
begin
  Assert(Assigned(ALineParams));

  with VAppearance do begin
    // line color
    with ARedefinitions.FLineColorDef do begin
      if FFound then
        FLineColor := FColor
      else
        FLineColor := ALineParams.LineAppearance.LineColor;
    end;

    // line width
    FLineWidth := ARedefinitions.FLineWidth;
    if FLineWidth = 0 then begin
      FLineWidth := ALineParams.LineAppearance.LineWidth;
    end;
  end;

  Result := LoadPolylineAppearance(@VAppearance);
end;

function TAppearanceHelper.RedefinePointAppearance(
  const APointParams: IImportPointParams;
  const ARedefinitions: TAppearanceRedefinitions;
  const ARedefinePic: IMarkPicture
): IAppearance;
var
  VAppearance: TPointAppearanceData;
  VPic: IMarkPicture;
  VPicName: string;
begin
  Assert(Assigned(APointParams));

  VPic := ARedefinePic;
  if not Assigned(VPic) then begin
    VPic := APointParams.IconAppearance.Pic;
  end;

  if Assigned(VPic) then begin
    VPicName := VPic.GetName;
  end else begin
    VPicName := '';
  end;

  with VAppearance do begin
    // fill color
    with ARedefinitions.FTextColorDef do begin
      if FFound then
        FTextColor := FColor
      else
        FTextColor := APointParams.CaptionAppearance.TextColor;
    end;

    // back color
    with ARedefinitions.FFillColorDef do begin
      if FFound then
        FTextBgColor := FColor
      else
        FTextBgColor := APointParams.CaptionAppearance.TextBgColor;
    end;

    // font size
    FFontSize := ARedefinitions.FTextSize;
    if FFontSize = 0 then begin
      FFontSize := APointParams.CaptionAppearance.FontSize;
    end;

    // marker size
    FMarkerSize := ARedefinitions.FIconSize;
    if FMarkerSize = 0 then begin
      FMarkerSize := APointParams.IconAppearance.MarkerSize;
    end;
  end;

  Result := LoadPointAppearance(
    @VAppearance,
    VPicName,
    VPic
  );
end;

function TAppearanceHelper.RedefinePolygonAppearance(
  const APolygonParams: IImportPolyParams;
  const ARedefinitions: TAppearanceRedefinitions
): IAppearance;
var
  VAppearance: TPolygonAppearanceData;
begin
  Assert(Assigned(APolygonParams));

  with VAppearance.FLineAppearance do begin
    // border color
    with ARedefinitions.FLineColorDef do begin
      if FFound then
        FLineColor := FColor
      else
        FLineColor := APolygonParams.BorderAppearance.LineColor;
    end;

    // line width
    FLineWidth := ARedefinitions.FLineWidth;
    if FLineWidth = 0 then begin
      FLineWidth := APolygonParams.BorderAppearance.LineWidth;
    end;
  end;

  with VAppearance do begin
    // fill color
    with ARedefinitions.FFillColorDef do begin
      if FFound then
        FFillColor := FColor
      else
        FFillColor := APolygonParams.FillAppearance.FillColor;
    end;
  end;

  Result := LoadPolygonAppearance(@VAppearance);
end;

end.