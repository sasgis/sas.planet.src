unit u_MarkPolyTemplateConfig;

interface
uses
  Classes,
  GR32,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_MarksSimple,
  i_IMarksFactoryConfig,
  u_ConfigDataElementBase;

type
  TMarkPolyTemplateConfig = class(TConfigDataElementBase, IMarkPolyTemplateConfig)
  private
    FDefaultTemplate: IMarkTemplatePoly;
    function IsSameTempalte(lhs, rhs: IMarkTemplatePoly): Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function CreateTemplate(
      ACategoryId: Integer;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer
    ): IMarkTemplatePoly; overload;
    function CreateTemplate(
      ASource: IMarkFull
    ): IMarkTemplatePoly; overload;

    function GetDefaultTemplate: IMarkTemplatePoly;
    procedure SetDefaultTemplate(AValue: IMarkTemplatePoly);
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigProviderHelpers,
  u_MarkTemplates;

{ TMarkPolyTemplateConfig }

constructor TMarkPolyTemplateConfig.Create;
begin
  inherited Create;
  FDefaultTemplate := CreateTemplate(
    -1,
    SetAlpha(clBlack32, 166),
    SetAlpha(clWhite32, 51),
    2
  );
end;

function TMarkPolyTemplateConfig.CreateTemplate(
  ACategoryId: Integer;
  AColor1: TColor32;
  AColor2: TColor32;
  AScale1: Integer
): IMarkTemplatePoly;
begin
  Result := TMarkTemplatePoly.Create(
    ACategoryId,
    AColor1,
    AColor2,
    AScale1
  );
end;

function TMarkPolyTemplateConfig.CreateTemplate(
  ASource: IMarkFull): IMarkTemplatePoly;
begin
  Result := CreateTemplate(
    ASource.CategoryId,
    ASource.Color1,
    ASource.Color2,
    ASource.Scale1
  );
end;

procedure TMarkPolyTemplateConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
var
  VCategoryId: Integer;
  VColor1, VColor2: TColor32;
  VScale1: Integer;
begin
  inherited;
  VCategoryId := FDefaultTemplate.CategoryId;
  VColor1 := FDefaultTemplate.Color1;
  VColor2 := FDefaultTemplate.Color2;
  VScale1 := FDefaultTemplate.Scale1;
  if AConfigData <> nil then begin
    VCategoryId := AConfigData.ReadInteger('CategoryId', VCategoryId);
    VColor1 := LoadColor32(AConfigData, 'LineColor', VColor1);
    VColor2 := LoadColor32(AConfigData, 'FillColor', VColor2);
    VScale1 := AConfigData.ReadInteger('LineWidth', VScale1);
  end;
  SetDefaultTemplate(
    CreateTemplate(
      VCategoryId,
      VColor1,
      VColor2,
      VScale1
    )
  );
end;

procedure TMarkPolyTemplateConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('CategoryId', FDefaultTemplate.CategoryId);
  WriteColor32(AConfigData, 'LineColor', FDefaultTemplate.Color1);
  WriteColor32(AConfigData, 'FillColor', FDefaultTemplate.Color2);
  AConfigData.WriteInteger('LineWidth', FDefaultTemplate.Scale1);
end;

function TMarkPolyTemplateConfig.GetDefaultTemplate: IMarkTemplatePoly;
begin
  LockRead;
  try
    Result := FDefaultTemplate;
  finally
    UnlockRead;
  end;
end;

function TMarkPolyTemplateConfig.IsSameTempalte(lhs,
  rhs: IMarkTemplatePoly): Boolean;
begin
  Result :=
    (lhs.CategoryId = rhs.CategoryId) and
    (lhs.Color1 = rhs.Color1) and
    (lhs.Color2 = rhs.Color2) and
    (lhs.Scale1 = rhs.Scale1)
end;

procedure TMarkPolyTemplateConfig.SetDefaultTemplate(
  AValue: IMarkTemplatePoly);
begin
  LockWrite;
  try
    if not IsSameTempalte(FDefaultTemplate, AValue) then begin
      FDefaultTemplate := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.

