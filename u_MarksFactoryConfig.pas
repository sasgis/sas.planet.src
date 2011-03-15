unit u_MarksFactoryConfig;

interface

uses
  Classes,
  GR32,
  i_IMarkPicture,
  i_IMarksFactoryConfig,
  u_ConfigDataElementComplexBase;

type
  TMarksFactoryConfig = class(TConfigDataElementComplexBase, IMarksFactoryConfig)
  private
    FPointTemplateConfig: IMarkPointTemplateConfig;
    FLineTemplateConfig: IMarkLineTemplateConfig;
    FPolyTemplateConfig: IMarkPolyTemplateConfig;
  protected
    function GetPointTemplateConfig: IMarkPointTemplateConfig;
    function GetLineTemplateConfig: IMarkLineTemplateConfig;
    function GetPolyTemplateConfig: IMarkPolyTemplateConfig;
  public
    constructor Create(AMarkPictureList: IMarkPictureList);
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_MarkPointTemplateConfig,
  u_MarkLineTemplateConfig,
  u_MarkPolyTemplateConfig;

{ TMarksFactoryConfig }

constructor TMarksFactoryConfig.Create(AMarkPictureList: IMarkPictureList);
begin
  inherited Create;

  FPointTemplateConfig := TMarkPointTemplateConfig.Create(AMarkPictureList);
  Add(FPointTemplateConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarkNewPoint'));

  FLineTemplateConfig := TMarkLineTemplateConfig.Create;
  Add(FLineTemplateConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarkNewLine'));

  FPolyTemplateConfig := TMarkPolyTemplateConfig.Create;
  Add(FPolyTemplateConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarkNewPoly'));
end;

function TMarksFactoryConfig.GetLineTemplateConfig: IMarkLineTemplateConfig;
begin
  Result := FLineTemplateConfig;
end;

function TMarksFactoryConfig.GetPointTemplateConfig: IMarkPointTemplateConfig;
begin
  Result := FPointTemplateConfig;
end;

function TMarksFactoryConfig.GetPolyTemplateConfig: IMarkPolyTemplateConfig;
begin
  Result := FPolyTemplateConfig;
end;

end.
