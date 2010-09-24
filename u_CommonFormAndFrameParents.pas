unit u_CommonFormAndFrameParents;

interface

uses
  Classes,
  Forms;

type
  TCommonFormParent = class(TForm)
  public
    constructor Create(AOwner : TComponent); override;
    procedure RefreshTranslation; virtual;
  end;

  TCommonFrameParent = class(Forms.TFrame)
  public
    constructor Create(AOwner : TComponent); override;
    procedure RefreshTranslation; virtual;
  end;

  TFrame = class(TCommonFrameParent);

implementation

uses
  gnugettext;

{ TCommonFormParent }

constructor TCommonFormParent.Create(AOwner: TComponent);
begin
  inherited;
  TranslateComponent(self);
end;

procedure TCommonFormParent.RefreshTranslation;
begin
  ReTranslateComponent(self);
end;

{ TFrame }

constructor TCommonFrameParent.Create(AOwner: TComponent);
begin
  inherited;
  if (Owner = Application) or (Owner = nil) then begin
    TranslateComponent(self);
  end;
end;

procedure TCommonFrameParent.RefreshTranslation;
begin
  if (Owner = Application) or (Owner = nil) then begin
    ReTranslateComponent(self);
  end;
end;

end.
