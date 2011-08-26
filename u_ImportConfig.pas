unit u_ImportConfig;

interface

uses
  i_MarkTemplate,
  i_ImportConfig,
  i_MarksDb;

type
  TImportConfig = class(TInterfacedObject, IImportConfig)
  private
    FMarkDB: IMarksDb;
    FTemplateNewPoint: IMarkTemplatePoint;
    FTemplateNewLine: IMarkTemplateLine;
    FTemplateNewPoly: IMarkTemplatePoly;
  protected
    function GetMarkDB: IMarksDb;
    function GetTemplateNewPoint: IMarkTemplatePoint;
    function GetTemplateNewLine: IMarkTemplateLine;
    function GetTemplateNewPoly: IMarkTemplatePoly;
  public
    constructor Create(
      AMarkDB: IMarksDb;
      ATemplateNewPoint: IMarkTemplatePoint;
      ATemplateNewLine: IMarkTemplateLine;
      ATemplateNewPoly: IMarkTemplatePoly
    );
  end;

implementation

{ TImportConfig }

constructor TImportConfig.Create(
  AMarkDB: IMarksDb;
  ATemplateNewPoint: IMarkTemplatePoint;
  ATemplateNewLine: IMarkTemplateLine;
  ATemplateNewPoly: IMarkTemplatePoly
);
begin
  FMarkDB := AMarkDB;
  FTemplateNewPoint := ATemplateNewPoint;
  FTemplateNewLine := ATemplateNewLine;
  FTemplateNewPoly := ATemplateNewPoly;
end;

function TImportConfig.GetMarkDB: IMarksDb;
begin
  Result := FMarkDB;
end;

function TImportConfig.GetTemplateNewLine: IMarkTemplateLine;
begin
  Result := FTemplateNewLine;
end;

function TImportConfig.GetTemplateNewPoint: IMarkTemplatePoint;
begin
  Result := FTemplateNewPoint;
end;

function TImportConfig.GetTemplateNewPoly: IMarkTemplatePoly;
begin
  Result := FTemplateNewPoly;
end;

end.
