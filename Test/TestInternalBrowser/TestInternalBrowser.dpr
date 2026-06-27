program TestInternalBrowser;

uses
  Vcl.Forms,
  frm_Main in 'frm_Main.pas' {frmMain},
  u_InternalBrowserImplByEdge in '..\..\Src\Browser\u_InternalBrowserImplByEdge.pas',
  u_InternalBrowserImplByIE in '..\..\Src\Browser\u_InternalBrowserImplByIE.pas',
  u_InternalBrowserImpl in '..\..\Src\Browser\u_InternalBrowserImpl.pas',
  u_InternalBrowserFactory in '..\..\Src\Browser\u_InternalBrowserFactory.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
