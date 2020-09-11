unit Form.Main;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.Classes, FGX.Forms, FGX.Forms.Types, FGX.Controls, FGX.Controls.Types, FGX.Layout,
  FGX.Layout.Types, FGX.NavigationBar.Types, FGX.Button.Types, FGX.Button, FGX.Memo, FGX.NavigationBar;

type
  TFormMain = class(TfgForm)
    fgNavigationBar1: TfgNavigationBar;
    fgMemo1: TfgMemo;
    fgButton1: TfgButton;
    fgLayout1: TfgLayout;
    fgLayout2: TfgLayout;
    procedure fgButton1Tap(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.xfm}

uses
  System.SysUtils, FGX.Application, FGX.Dialogs, FGX.Log, FGX.ConnectionChecker.Android;

procedure TFormMain.fgButton1Tap(Sender: TObject);
begin
  fgMemo1.Lines.Clear;
  fgMemo1.Lines.Add('CheckPermission: ' + BoolToStr(TConnectionChecker.CheckPermission, True));
  if TConnectionChecker.CheckPermission then
  begin
    fgMemo1.Lines.Add('IsConnected: ' + BoolToStr(TConnectionChecker.IsConnected, True));
    fgMemo1.Lines.Add('IsEthernet: ' + BoolToStr(TConnectionChecker.HasActiveNetworkConnection
      (TConnectionType.Ethernet), True));
    fgMemo1.Lines.Add('IsMobile: ' + BoolToStr(TConnectionChecker.HasActiveNetworkConnection
      (TConnectionType.Mobile), True));
    fgMemo1.Lines.Add('IsWifi: ' + BoolToStr(TConnectionChecker.HasActiveNetworkConnection
      (TConnectionType.Wifi), True));
    fgMemo1.Lines.Add('IsVPN: ' + BoolToStr(TConnectionChecker.HasActiveNetworkConnection(TConnectionType.VPN), True));
    fgMemo1.Lines.Add('HasInternet: ' + BoolToStr(TConnectionChecker.HasInternet, True));
  end;
end;

end.
