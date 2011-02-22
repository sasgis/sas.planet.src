unit u_MarkLineTemplateConfig;

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
  TMarkLineTemplateConfig = class(TConfigDataElementBase, IMarkLineTemplateConfig)
  private
    FDefaultTemplate: IMarkTemplateLine;
    function IsSameTempalte(lhs, rhs: IMarkTemplateLine): Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function CreateTemplate(
      ACategoryId: Integer;
      AColor1: TColor32;
      AScale1: Integer
    ): IMarkTemplateLine; overload;
    function CreateTemplate(
      ASource: IMarkFull
    ): IMarkTemplateLine; overload;

    function GetDefaultTemplate: IMarkTemplateLine;
    procedure SetDefaultTemplate(AValue: IMarkTemplateLine);
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigProviderHelpers,
  u_MarkTemplates;

{ TMarkLineTemplateConfig }

constructor TMarkLineTemplateConfig.Create;
begin
  inherited;
  FDefaultTemplate := CreateTemplate(
    -1,
    SetAlpha(clRed32, 166),
    2
  );
end;

function TMarkLineTemplateConfig.CreateTemplate(
  ACategoryId: Integer;
  AColor1: TColor32;
  AScale1: Integer
): IMarkTemplateLine;
begin
  Result := TMarkTemplateLine.Create(
    ACategoryId,
    AColor1,
    AScale1
  );
end;

function TMarkLineTemplateConfig.CreateTemplate(
  ASource: IMarkFull): IMarkTemplateLine;
begin
  Result := CreateTemplate(
    ASource.CategoryId,
    ASource.Color1,
    ASource.Scale1
  );
end;

procedure TMarkLineTemplateConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
var
  VCategoryId: Integer;
  VColor1: TColor32;
  VScale1: Integer;
begin
  inherited;
  VCategoryId := FDefaultTemplate.CategoryId;
  VColor1 := FDefaultTemplate.Color1;
  VScale1 := FDefaultTemplate.Scale1;
  if AConfigData <> nil then begin
    VCategoryId := AConfigData.ReadInteger('CategoryId', VCategoryId);
    VColor1 := LoadColor32(AConfigData, 'LineColor', VColor1);
    VScale1 := AConfigData.ReadInteger('LineWidth', VScale1);
  end;
  SetDefaultTemplate(
    CreateTemplate(
      VCategoryId,
      VColor1,
      VScale1
    )
  );
end;

procedure TMarkLineTemplateConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('CategoryId', FDefaultTemplate.CategoryId);
  WriteColor32(AConfigData, 'LineColor', FDefaultTemplate.Color1);
  AConfigData.WriteInteger('LineWidth', FDefaultTemplate.Scale1);
end;

function TMarkLineTemplateConfig.GetDefaultTemplate: IMarkTemplateLine;
begin
  LockRead;
  try
    Result := FDefaultTemplate;
  finally
    UnlockRead;
  end;
end;

function TMarkLineTemplateConfig.IsSameTempalte(lhs,
  rhs: IMarkTemplateLine): Boolean;
begin
  Result :=
    (lhs.CategoryId = rhs.CategoryId) and
    (lhs.Color1 = rhs.Color1) and
    (lhs.Scale1 = rhs.Scale1);
end;

procedure TMarkLineTemplateConfig.SetDefaultTemplate(
  AValue: IMarkTemplateLine);
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

