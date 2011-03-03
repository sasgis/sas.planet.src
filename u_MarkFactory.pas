unit u_MarkFactory;

interface

uses
  Windows,
  GR32,
  t_GeoTypes,
  i_IMarkPicture,
  i_IMarksFactoryConfig,
  i_MarksSimple;

type

  TMarkFactory =  class
  private
    FConfig: IMarksFactoryConfig;
    FNewPointCounter: Integer;
    FNewLineCounter: Integer;
    FNewPolyCounter: Integer;

    FMarkPictureList: IMarkPictureList;
    function GetLLRectFromPoints(APoints: TArrayOfDoublePoint): TDoubleRect;
    function GetLLRectFromPoint(APoint: TDoublePoint): TDoubleRect;
    procedure ClosePolyPoints(var APoints: TArrayOfDoublePoint);
  public
    function CreateNewPoint(
      APoint: TDoublePoint;
      AName: string;
      ADesc: string;
      ATemplate: IMarkTemplatePoint = nil
    ): IMarkFull;
    function CreateNewLine(
      APoints: TArrayOfDoublePoint;
      AName: string;
      ADesc: string;
      ATemplate: IMarkTemplateLine = nil
    ): IMarkFull;
    function CreateNewPoly(
      APoints: TArrayOfDoublePoint;
      AName: string;
      ADesc: string;
      ATemplate: IMarkTemplatePoly = nil
    ): IMarkFull;

    function CreatePoint(
      AID: Integer;
      AName: string;
      AVisible: Boolean;
      APicName: string;
      ACategoryId: Integer;
      ADesc: string;
      APoint: TDoublePoint;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer
    ): IMarkFull; overload;
    function CreatePoint(
      AName: string;
      AVisible: Boolean;
      APicName: string;
      APic: IMarkPicture;
      ACategoryId: Integer;
      ADesc: string;
      APoint: TDoublePoint;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer;
      ASource: IMarkFull
    ): IMarkFull; overload;
    function CreateLine(
      AID: Integer;
      AName: string;
      AVisible: Boolean;
      ACategoryId: Integer;
      ADesc: string;
      APoints: TArrayOfDoublePoint;
      AColor1: TColor32;
      AScale1: Integer
    ): IMarkFull; overload;
    function CreateLine(
      AName: string;
      AVisible: Boolean;
      ACategoryId: Integer;
      ADesc: string;
      APoints: TArrayOfDoublePoint;
      AColor1: TColor32;
      AScale1: Integer;
      ASource: IMarkFull
    ): IMarkFull; overload;
    function CreatePoly(
      AID: Integer;
      AName: string;
      AVisible: Boolean;
      ACategoryId: Integer;
      ADesc: string;
      APoints: TArrayOfDoublePoint;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer
    ): IMarkFull; overload;
    function CreatePoly(
      AName: string;
      AVisible: Boolean;
      ACategoryId: Integer;
      ADesc: string;
      APoints: TArrayOfDoublePoint;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      ASource: IMarkFull
    ): IMarkFull; overload;

    function CreateModifedLine(
      APoints: TArrayOfDoublePoint;
      ADesc: string;
      ASource: IMarkFull
    ): IMarkFull;
    function CreateModifedPoly(
      APoints: TArrayOfDoublePoint;
      ASource: IMarkFull
    ): IMarkFull;
    property Config: IMarksFactoryConfig read FConfig;
  public
    constructor Create(AConfig: IMarksFactoryConfig);
  end;

implementation

uses
  Graphics,
  SysUtils,
  UResStrings,
  Ugeofun,
  u_MarkTemplates,
  u_MarkPoint,
  u_MarkLine,
  u_MarkPoly;

{ TMarkFactory }

constructor TMarkFactory.Create(AConfig: IMarksFactoryConfig);
begin
  FConfig := AConfig;

  FMarkPictureList := FConfig.PointTemplateConfig.MarkPictureList;
  FNewPointCounter := 0;
  FNewLineCounter := 0;
  FNewPolyCounter := 0;
end;

function TMarkFactory.CreateNewLine(APoints: TArrayOfDoublePoint; AName,
  ADesc: string; ATemplate: IMarkTemplateLine): IMarkFull;
var
  VTemplate: IMarkTemplateLine;
  VName: string;
  VCounter: Integer;
begin
  VTemplate := ATemplate;
  if VTemplate = nil then begin
    VTemplate := FConfig.LineTemplateConfig.DefaultTemplate;
  end;

  VName := AName;
  if VName = '' then begin
    VCounter := InterlockedIncrement(FNewLineCounter);
    VName := Format('%0:s %.2d', [SAS_STR_NewPath, VCounter]);
  end;

  Result := CreateLine(
    VName,
    True,
    VTemplate.CategoryId,
    ADesc,
    APoints,
    VTemplate.Color1,
    VTemplate.Scale1,
    nil
  );
end;

function TMarkFactory.CreateNewPoint(APoint: TDoublePoint; AName, ADesc: string;
  ATemplate: IMarkTemplatePoint): IMarkFull;
var
  VTemplate: IMarkTemplatePoint;
  VName: string;
begin
  VTemplate := ATemplate;
  if VTemplate = nil then begin
    VTemplate := FConfig.PointTemplateConfig.DefaultTemplate;
  end;

  VName := AName;
  if VName = '' then begin
    VName := SAS_STR_NewMark;
  end;

  Result := CreatePoint(
    VName,
    True,
    VTemplate.PicName,
    VTemplate.Pic,
    VTemplate.CategoryId,
    ADesc,
    APoint,
    VTemplate.Color1,
    VTemplate.Color2,
    VTemplate.Scale1,
    VTemplate.Scale2,
    nil
  );
end;

function TMarkFactory.CreateNewPoly(APoints: TArrayOfDoublePoint; AName,
  ADesc: string; ATemplate: IMarkTemplatePoly): IMarkFull;
var
  VTemplate: IMarkTemplatePoly;
  VName: string;
begin
  VTemplate := ATemplate;
  if VTemplate = nil then begin
    VTemplate := FConfig.PolyTemplateConfig.TemplateDefault;
  end;

  VName := AName;
  if VName = '' then begin
    VName := SAS_STR_NewPoly;
  end;

  Result := CreatePoly(
    VName,
    True,
    VTemplate.CategoryId,
    ADesc,
    APoints,
    VTemplate.Color1,
    VTemplate.Color2,
    VTemplate.Scale1,
    nil
  );
end;

function TMarkFactory.CreateModifedLine(APoints: TArrayOfDoublePoint;
  ADesc: string; ASource: IMarkFull): IMarkFull;
var
  VDesc: string;
  VVisible: Boolean;
  VMarkVisible: IMarkVisible;
begin
  VVisible := True;
  if Supports(ASource, IMarkVisible, VMarkVisible) then begin
    VVisible := VMarkVisible.Visible;
  end;
  VDesc := ADesc;
  if ADesc = '' then begin
    VDesc := ASource.Desc;
  end;
  Result := CreateLine(
    ASource.Name,
    VVisible,
    ASource.CategoryId,
    VDesc,
    APoints,
    ASource.Color1,
    ASource.Scale1,
    ASource
  );
end;

function TMarkFactory.CreateModifedPoly(APoints: TArrayOfDoublePoint;
  ASource: IMarkFull): IMarkFull;
var
  VVisible: Boolean;
  VMarkVisible: IMarkVisible;
begin
  VVisible := True;
  if Supports(ASource, IMarkVisible, VMarkVisible) then begin
    VVisible := VMarkVisible.Visible;
  end;
  Result := CreatePoly(
    ASource.Name,
    VVisible,
    ASource.CategoryId,
    ASource.Desc,
    APoints,
    ASource.Color1,
    ASource.Color2,
    ASource.Scale1,
    ASource
  );
end;

function TMarkFactory.CreateLine(
  AName: string;
  AVisible: Boolean;
  ACategoryId: Integer;
  ADesc: string;
  APoints: TArrayOfDoublePoint;
  AColor1: TColor32;
  AScale1: Integer;
  ASource: IMarkFull
): IMarkFull;
var
  VID: Integer;
begin
  if ASource <> nil then begin
    VID := ASource.Id;
  end else begin
    VID := -1;
  end;
  Result := TMarkLine.Create(
    AName,
    VId,
    AVisible,
    ACategoryId,
    ADesc,
    GetLLRectFromPoints(APoints),
    APoints,
    AColor1,
    AScale1
  );
end;

function TMarkFactory.CreateLine(AID: Integer; AName: string; AVisible: Boolean;
  ACategoryId: Integer; ADesc: string; APoints: TArrayOfDoublePoint;
  AColor1: TColor32; AScale1: Integer): IMarkFull;
begin
  Result := TMarkLine.Create(
    AName,
    AId,
    AVisible,
    ACategoryId,
    ADesc,
    GetLLRectFromPoints(APoints),
    APoints,
    AColor1,
    AScale1
  );
end;

function TMarkFactory.CreatePoint(
  AName: string;
  AVisible: Boolean;
  APicName: string;
  APic: IMarkPicture;
  ACategoryId: Integer;
  ADesc: string;
  APoint: TDoublePoint;
  AColor1: TColor32;
  AColor2: TColor32;
  AScale1: Integer;
  AScale2: Integer;
  ASource: IMarkFull
): IMarkFull;
var
  VID: Integer;
begin
  if ASource <> nil then begin
    VID := ASource.Id;
  end else begin
    VID := -1;
  end;
  Result := TMarkPoint.Create(
    AName,
    VID,
    AVisible,
    APicName,
    APic,
    ACategoryId,
    ADesc,
    GetLLRectFromPoint(APoint),
    APoint,
    AColor1,
    AColor2,
    AScale1,
    AScale2
  );
end;

function TMarkFactory.CreatePoint(
  AID: Integer;
  AName: string;
  AVisible: Boolean;
  APicName: string;
  ACategoryId: Integer;
  ADesc: string;
  APoint: TDoublePoint;
  AColor1, AColor2: TColor32;
  AScale1, AScale2: Integer
): IMarkFull;
var
  VPicIndex: Integer;
  VPic: IMarkPicture;
begin
  VPicIndex := FMarkPictureList.GetIndexByName(APicName);
  if VPicIndex < 0 then begin
    VPic := nil;
  end else begin
    VPic := FMarkPictureList.Get(VPicIndex);
  end;
  Result := TMarkPoint.Create(
    AName,
    AID,
    AVisible,
    APicName,
    VPic,
    ACategoryId,
    ADesc,
    GetLLRectFromPoint(APoint),
    APoint,
    AColor1,
    AColor2,
    AScale1,
    AScale2
  );
end;

function TMarkFactory.CreatePoly(AID: Integer; AName: string; AVisible: Boolean;
  ACategoryId: Integer; ADesc: string; APoints: TArrayOfDoublePoint; AColor1,
  AColor2: TColor32; AScale1: Integer): IMarkFull;
begin
  Result := TMarkPoly.Create(
    AName,
    AID,
    AVisible,
    ACategoryId,
    ADesc,
    GetLLRectFromPoints(APoints),
    APoints,
    AColor1,
    AColor2,
    AScale1
  );
end;

function TMarkFactory.CreatePoly(
  AName: string;
  AVisible: Boolean;
  ACategoryId: Integer;
  ADesc: string;
  APoints: TArrayOfDoublePoint;
  AColor1: TColor32;
  AColor2: TColor32;
  AScale1: Integer;
  ASource: IMarkFull
): IMarkFull;
var
  VID: Integer;
  VPoints: TArrayOfDoublePoint;
begin
  if ASource <> nil then begin
    VID := ASource.Id;
  end else begin
    VID := -1;
  end;
  VPoints := Copy(APoints);
  ClosePolyPoints(VPoints);
  Result := TMarkPoly.Create(
    AName,
    VID,
    AVisible,
    ACategoryId,
    ADesc,
    GetLLRectFromPoints(APoints),
    VPoints,
    AColor1,
    AColor2,
    AScale1
  );
end;

function TMarkFactory.GetLLRectFromPoint(APoint: TDoublePoint): TDoubleRect;
begin
  Result.TopLeft := APoint;
  Result.BottomRight := APoint;
end;

function TMarkFactory.GetLLRectFromPoints(
  APoints: TArrayOfDoublePoint): TDoubleRect;
var
  VCount: Integer;
  i: Integer;
begin
  VCount := Length(APoints);
  if VCount > 0 then begin
    Result.TopLeft := APoints[0];
    Result.BottomRight := APoints[0];
    for i := 1 to VCount - 1 do begin
      if Result.Left > APoints[i].X then begin
        Result.Left := APoints[i].X;
      end;
      if Result.Top < APoints[i].Y then begin
        Result.Top := APoints[i].Y;
      end;
      if Result.Right < APoints[i].X then begin
        Result.Right := APoints[i].X;
      end;
      if Result.Bottom > APoints[i].Y then begin
        Result.Bottom := APoints[i].Y;
      end;
    end;
  end else begin
    Result.Left := 0;
    Result.Top := 0;
    Result.Right := 0;
    Result.Bottom := 0;
  end;
end;

procedure TMarkFactory.ClosePolyPoints(var APoints: TArrayOfDoublePoint);
var
  VCount: Integer;
begin
  VCount := Length(APoints);
  Assert(VCount > 2, 'В полигоне должно быть хотя бы 3 точки');
  if VCount > 0 then begin
    if not compare2EP(APoints[0], APoints[VCount - 1]) then begin
      SetLength(APoints, VCount + 1);
      APoints[VCount] := APoints[0];
    end;
  end;
end;

end.
