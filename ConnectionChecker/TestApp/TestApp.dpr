program TestApp;

uses
  FGX.Application,
  FGX.Forms,
  Form.Main in 'Form.Main.pas' {Form.Main: TfgForm},
  FGX.ConnectionChecker.Android in '..\class\FGX.ConnectionChecker.Android.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
