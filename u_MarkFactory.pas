unit u_MarkFactory;

interface

uses
  Windows,
  GR32,
  t_GeoTypes,
  i_MarkPicture,
  i_MarksFactoryConfig,
  i_MarkCategory,
  i_MarksSimple,
  i_MarkFactory,
  i_MarkFactoryDbInternal;

type

  TMarkFactory =  class(TInterfacedObject, IMarkFactory, IMarkFactoryDbInternal)
  private
    FConfig: IMarksFactoryConfig;

    FMarkPictureList: IMarkPictureList;
    function GetLLRectFromPoints(APoints: TArrayOfDoublePoint): TDoubleRect;
    function GetLLRectFromPoint(APoint: TDoublePoint): TDoubleRect;
    procedure PreparePolyPoints(var APoints: TArrayOfDoublePoint);
    procedure PreparePathPoints(var APoints: TArrayOfDoublePoint);

    function CreateLine(
      AID: Integer;
      AName: string;
      AVisible: Boolean;
      ACategoryId: Integer;
      ADesc: string;
      ARect: TDoubleRect;
      APoints: TArrayOfDoublePoint;
      AColor1: TColor32;
      AScale1: Integer
    ): IMarkFull;
    function CreatePoint(
      AID: Integer;
      AName: string;
      AVisible: Boolean;
      APicName: string;
      APic: IMarkPicture;
      ACategoryId: Integer;
      ADesc: string;
      APoint: TDoublePoint;
      AColor1, AColor2: TColor32;
      AScale1, AScale2: Integer
    ): IMarkFull;
    function CreatePoly(
      AID: Integer;
      AName: string;
      AVisible: Boolean;
      ACategoryId: Integer;
      ADesc: string;
      ARect: TDoubleRect;
      APoints: TArrayOfDoublePoint;
      AColor1, AColor2: TColor32;
      AScale1: Integer
    ): IMarkFull;
  protected
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

    function ModifyPoint(
      ASource: IMarkFull;
      AName: string;
      AVisible: Boolean;
      APicName: string;
      APic: IMarkPicture;
      ACategory: IMarkCategory;
      ADesc: string;
      APoint: TDoublePoint;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer
    ): IMarkFull;
    function ModifyLine(
      ASource: IMarkFull;
      AName: string;
      AVisible: Boolean;
      ACategory: IMarkCategory;
      ADesc: string;
      APoints: TArrayOfDoublePoint;
      AColor1: TColor32;
      AScale1: Integer
    ): IMarkFull;
    function ModifyPoly(
      ASource: IMarkFull;
      AName: string;
      AVisible: Boolean;
      ACategory: IMarkCategory;
      ADesc: string;
      APoints: TArrayOfDoublePoint;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer
    ): IMarkFull;

    function SimpleModifyLine(
      ASource: IMarkFull;
      APoints: TArrayOfDoublePoint;
      ADesc: string
    ): IMarkFull;
    function SimpleModifyPoly(
      ASource: IMarkFull;
      APoints: TArrayOfDoublePoint
    ): IMarkFull;

    function GetConfig: IMarksFactoryConfig;
  protected
    function CreateMark(
      AID: Integer;
      AName: string;
      AVisible: Boolean;
      APicName: string;
      ACategoryId: Integer;
      ADesc: string;
      ARect: TDoubleRect;
      APoints: TArrayOfDoublePoint;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer
    ): IMarkFull;
  public
    constructor Create(AConfig: IMarksFactoryConfig);
  end;

implementation

uses
  Graphics,
  SysUtils,
  UResStrings,
  Ugeofun,
  u_MarkPoint,
  u_MarkLine,
  u_MarkPoly;

{ TMarkFactory }

constructor TMarkFactory.Create(AConfig: IMarksFactoryConfig);
begin
  FConfig := AConfig;
  FMarkPictureList := FConfig.PointTemplateConfig.MarkPictureList;
end;

function TMarkFactory.CreateNewLine(APoints: TArrayOfDoublePoint; AName,
  ADesc: string; ATemplate: IMarkTemplateLine): IMarkFull;
var
  VTemplate: IMarkTemplateLine;
  VName: string;
  VPoints: TArrayOfDoublePoint;
begin
  VTemplate := ATemplate;
  if VTemplate = nil then begin
    VTemplate := FConfig.LineTemplateConfig.DefaultTemplate;
  end;

  VName := AName;
  if VName = '' then begin
    VName := VTemplate.GetNewName;
  end;

  VPoints := Copy(APoints);
  PreparePathPoints(VPoints);

  Result := CreateLine(
    -1,
    VName,
    True,
    VTemplate.CategoryId,
    ADesc,
    GetLLRectFromPoints(VPoints),
    VPoints,
    VTemplate.Color1,
    VTemplate.Scale1
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
    VName := VTemplate.GetNewName;
  end;

  Result := CreatePoint(
    -1,
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
    VTemplate.Scale2
  );
end;

function TMarkFactory.CreateNewPoly(APoints: TArrayOfDoublePoint; AName,
  ADesc: string; ATemplate: IMarkTemplatePoly): IMarkFull;
var
  VTemplate: IMarkTemplatePoly;
  VName: string;
  VPoints: TArrayOfDoublePoint;
begin
  VTemplate := ATemplate;
  if VTemplate = nil then begin
    VTemplate := FConfig.PolyTemplateConfig.TemplateDefault;
  end;

  VName := AName;
  if VName = '' then begin
    VName := VTemplate.GetNewName;
  end;

  VPoints := Copy(APoints);
  PreparePolyPoints(VPoints);

  Result := CreatePoly(
    -1,
    VName,
    True,
    VTemplate.CategoryId,
    ADesc,
    GetLLRectFromPoints(APoints),
    VPoints,
    VTemplate.Color1,
    VTemplate.Color2,
    VTemplate.Scale1
  );
end;

function TMarkFactory.CreatePoint(
  AID: Integer;
  AName: string;
  AVisible: Boolean;
  APicName: string;
  APic: IMarkPicture;
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
  VPic := APic;
  if VPic = nil then begin
    VPicIndex := FMarkPictureList.GetIndexByName(APicName);
    if VPicIndex < 0 then begin
      VPic := nil;
    end else begin
      VPic := FMarkPictureList.Get(VPicIndex);
    end;
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

function TMarkFactory.CreatePoly(
  AID: Integer; AName: string; AVisible: Boolean;
  ACategoryId: Integer; ADesc: string;
  ARect: TDoubleRect;
  APoints: TArrayOfDoublePoint; AColor1,
  AColor2: TColor32; AScale1: Integer): IMarkFull;
begin
  Result := TMarkPoly.Create(
    AName,
    AID,
    AVisible,
    ACategoryId,
    ADesc,
    ARect,
    APoints,
    AColor1,
    AColor2,
    AScale1
  );
end;

function TMarkFactory.CreateMark(AID: Integer; AName: string; AVisible: Boolean;
  APicName: string; ACategoryId: Integer; ADesc: string;
  ARect: TDoubleRect;
  APoints: TArrayOfDoublePoint;
  AColor1, AColor2: TColor32; AScale1, AScale2: Integer): IMarkFull;
var
  VPointCount: Integer;
begin
  VPointCount := Length(APoints);
  if VPointCount > 0 then begin
    if VPointCount = 1 then begin
      Result := CreatePoint(AId, AName, AVisible, APicName, nil, ACategoryId, ADesc, APoints[0], AColor1, AColor2, AScale1, AScale2)
    end else begin
      if compare2EP(APoints[0], APoints[VPointCount - 1]) then begin
        Result := CreatePoly(AId, AName, AVisible, ACategoryId, ADesc, ARect, APoints, AColor1, AColor2, AScale1);
      end else begin
        Result := CreateLine(AId, AName, AVisible, ACategoryId, ADesc, ARect, APoints, AColor1, AScale1);
      end;
    end;
  end;
end;

function TMarkFactory.SimpleModifyLine(
  ASource: IMarkFull;
  APoints: TArrayOfDoublePoint;
  ADesc: string
): IMarkFull;
var
  VDesc: string;
  VVisible: Boolean;
  VMarkVisible: IMarkVisible;
  VPoints: TArrayOfDoublePoint;
begin
  VVisible := True;
  if Supports(ASource, IMarkVisible, VMarkVisible) then begin
    VVisible := VMarkVisible.Visible;
  end;
  VDesc := ADesc;
  if ADesc = '' then begin
    VDesc := ASource.Desc;
  end;

  VPoints := Copy(APoints);
  PreparePathPoints(VPoints);

  Result := CreateLine(
    ASource.Id,
    ASource.Name,
    VVisible,
    ASource.CategoryId,
    VDesc,
    GetLLRectFromPoints(VPoints),
    VPoints,
    ASource.Color1,
    ASource.Scale1
  );
end;

function TMarkFactory.SimpleModifyPoly(
  ASource: IMarkFull;
  APoints: TArrayOfDoublePoint
): IMarkFull;
var
  VVisible: Boolean;
  VMarkVisible: IMarkVisible;
  VPoints: TArrayOfDoublePoint;
begin
  VVisible := True;
  if Supports(ASource, IMarkVisible, VMarkVisible) then begin
    VVisible := VMarkVisible.Visible;
  end;

  VPoints := Copy(APoints);
  PreparePathPoints(VPoints);

  Result := CreatePoly(
    ASource.Id,
    ASource.Name,
    VVisible,
    ASource.CategoryId,
    ASource.Desc,
    GetLLRectFromPoints(VPoints),
    VPoints,
    ASource.Color1,
    ASource.Color2,
    ASource.Scale1
  );
end;

function TMarkFactory.CreateLine(
  AID: Integer; AName: string; AVisible: Boolean;
  ACategoryId: Integer;
  ADesc: string;
  ARect: TDoubleRect;
  APoints: TArrayOfDoublePoint;
  AColor1: TColor32; AScale1: Integer): IMarkFull;
begin
  Result := TMarkLine.Create(
    AName,
    AId,
    AVisible,
    ACategoryId,
    ADesc,
    ARect,
    APoints,
    AColor1,
    AScale1
  );
end;

function TMarkFactory.ModifyPoint(
  ASource: IMarkFull;
  AName: string;
  AVisible: Boolean;
  APicName: string;
  APic: IMarkPicture;
  ACategory: IMarkCategory;
  ADesc: string;
  APoint: TDoublePoint;
  AColor1: TColor32;
  AColor2: TColor32;
  AScale1: Integer;
  AScale2: Integer
): IMarkFull;
var
  VID: Integer;
  VCategoryId: Integer;
begin
  if ASource <> nil then begin
    VID := ASource.Id;
  end else begin
    VID := -1;
  end;
  if ACategory <> nil then begin
    VCategoryId := ACategory.Id;
  end else begin
    VCategoryId := -1;
  end;
  Result := CreatePoint(
    VID,
    AName,
    AVisible,
    APicName,
    APic,
    VCategoryId,
    ADesc,
    APoint,
    AColor1,
    AColor2,
    AScale1,
    AScale2
  );
end;

function TMarkFactory.ModifyLine(
  ASource: IMarkFull;
  AName: string;
  AVisible: Boolean;
  ACategory: IMarkCategory;
  ADesc: string;
  APoints: TArrayOfDoublePoint;
  AColor1: TColor32;
  AScale1: Integer
): IMarkFull;
var
  VID: Integer;
  VCategoryId: Integer;
  VPoints: TArrayOfDoublePoint;
begin
  if ASource <> nil then begin
    VID := ASource.Id;
  end else begin
    VID := -1;
  end;

  if ACategory <> nil then begin
    VCategoryId := ACategory.Id;
  end else begin
    VCategoryId := -1;
  end;

  VPoints := Copy(APoints);
  PreparePathPoints(VPoints);
  Result := CreateLine(
    VId,
    AName,
    AVisible,
    VCategoryId,
    ADesc,
    GetLLRectFromPoints(VPoints),
    VPoints,
    AColor1,
    AScale1
  );
end;

function TMarkFactory.ModifyPoly(
  ASource: IMarkFull;
  AName: string;
  AVisible: Boolean;
  ACategory: IMarkCategory;
  ADesc: string;
  APoints: TArrayOfDoublePoint;
  AColor1: TColor32;
  AColor2: TColor32;
  AScale1: Integer
): IMarkFull;
var
  VID: Integer;
  VCategoryId: Integer;
  VPoints: TArrayOfDoublePoint;
begin
  if ASource <> nil then begin
    VID := ASource.Id;
  end else begin
    VID := -1;
  end;

  if ACategory <> nil then begin
    VCategoryId := ACategory.Id;
  end else begin
    VCategoryId := -1;
  end;

  VPoints := Copy(APoints);
  PreparePolyPoints(VPoints);
  Result := CreatePoly(
    VID,
    AName,
    AVisible,
    VCategoryId,
    ADesc,
    GetLLRectFromPoints(VPoints),
    VPoints,
    AColor1,
    AColor2,
    AScale1
  );
end;

function TMarkFactory.GetConfig: IMarksFactoryConfig;
begin
  Result := FConfig;
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
      if not PointIsEmpty(APoints[i]) then begin
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
    end;
  end else begin
    Result.Left := 0;
    Result.Top := 0;
    Result.Right := 0;
    Result.Bottom := 0;
  end;
end;

procedure TMarkFactory.PreparePathPoints(var APoints: TArrayOfDoublePoint);
var
  VCount: Integer;
begin
  VCount := Length(APoints);
  while (VCount > 0) and PointIsEmpty(APoints[VCount - 1]) do begin
    Dec(VCount);
  end;
  SetLength(APoints, VCount);
  Assert(VCount > 1, 'В пути должно быть хотя бы 2 точки');
end;

procedure TMarkFactory.PreparePolyPoints(var APoints: TArrayOfDoublePoint);
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
