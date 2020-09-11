{ *********************************************************************
  *
  * Autor: Efimov A.A.
  * E-mail: infocean@gmail.com
  * GitHub: https://github.com/AndrewEfimov/FGX-Android-API-Examples
  * Description: Getting information about connecting.
  * Requirements: You need permission "ACCESS_NETWORK_STATE" in the manifest.
  * Platform: only Android (API 21 +) (Tested: API level 24/27/29)
  * IDE (Tested): Delphi 10.3.3+ and FGX 1.4.5.0+
  *
  ******************************************************************** }
unit FGX.ConnectionChecker.Android;

interface

{$WARN SYMBOL_DEPRECATED OFF}
{$SCOPEDENUMS ON}

uses
  Android.Api.Network, FGX.Permissions, FGX.Helpers.Android, Android.Api.ActivityAndView, Java.Bridge;

type
  TConnectionType = (Ethernet, Mobile, Wifi, VPN);

  TConnectionChecker = class
  private const
    APILevel = 23;
  public const
    AccessNetworkStatePermission = 'android.permission.ACCESS_NETWORK_STATE';
  private
    class var FConnectivityManager: JConnectivityManager;
    class constructor Create;
    /// <summary> Returns details about the currently active default data network. </summary>
    class function GetNetworkInfo: JNetworkInfo;
    /// <summary> Get the NetworkCapabilities for the given Network. </summary>
    class function GetNetworkCapabilities(const ANetwork: JNetwork): JNetworkCapabilities;
  public
    /// <summary> Check permission "android.permission.ACCESS_NETWORK_STATE" </summary>
    class function CheckPermission: Boolean;
    /// <summary> Indicates whether network connectivity exists and it is possible to establish connections and pass data. </summary>
    class function IsConnected: Boolean;
    /// <summary> Check the connection type. </summary>
    class function HasActiveNetworkConnection(const AType: TConnectionType): Boolean;
    /// <summary>
    /// This is an experimental version of the internet check method. It is not stable.
    /// Android API Support >= 23
    /// </summary>
    class function HasInternet: Boolean;
  end;

implementation

{ TActiveNetworkInfo }

class function TConnectionChecker.HasInternet: Boolean;
var
  Networks: IJavaArray<JNetwork>;
  I: Integer;
  NetworkCap: JNetworkCapabilities;
begin
  Result := False;

  if FConnectivityManager <> nil then
    if TJBuild_VERSION.SDK_INT >= APILevel then
    begin
      Networks := FConnectivityManager.getAllNetworks();

      if (Networks <> nil) then
        for I := 0 to Networks.Length - 1 do
        begin
          NetworkCap := GetNetworkCapabilities(Networks[I]);

          if (NetworkCap <> nil) and not NetworkCap.hasTransport(TJNetworkCapabilities.TRANSPORT_VPN) then
          begin
            Result := NetworkCap.hasCapability(TJNetworkCapabilities.NET_CAPABILITY_INTERNET) and
              (NetworkCap.hasCapability(TJNetworkCapabilities.NET_CAPABILITY_VALIDATED));
            if Result then
              break;
          end;
        end;
    end;
end;

class function TConnectionChecker.CheckPermission: Boolean;
var
  PermissionInfo: TfgPermissionInfo;
begin
  PermissionInfo := TfgPermissionService.CheckPermission(AccessNetworkStatePermission);
  Result := PermissionInfo.CheckResult = TPermissionCheckResult.Granted;
end;

class function TConnectionChecker.HasActiveNetworkConnection(const AType: TConnectionType): Boolean;
var
  NetworkCap: JNetworkCapabilities;
  TypeTransport: Integer;
  NetworkInfo: JNetworkInfo;
begin
  Result := False;

  if FConnectivityManager <> nil then
    if TJBuild_VERSION.SDK_INT >= APILevel then
    begin
      case AType of
        TConnectionType.Ethernet:
          TypeTransport := TJNetworkCapabilities.TRANSPORT_ETHERNET;
        TConnectionType.Mobile:
          TypeTransport := TJNetworkCapabilities.TRANSPORT_CELLULAR;
        TConnectionType.Wifi:
          TypeTransport := TJNetworkCapabilities.TRANSPORT_WIFI;
        TConnectionType.VPN:
          TypeTransport := TJNetworkCapabilities.TRANSPORT_VPN;
      else
        TypeTransport := -1;
      end;

      NetworkCap := GetNetworkCapabilities(FConnectivityManager.getActiveNetwork);
      if TypeTransport <> -1 then
        Result := (NetworkCap <> nil) and NetworkCap.hasTransport(TypeTransport);
    end
    else
    begin
      case AType of
        TConnectionType.Ethernet:
          TypeTransport := TJConnectivityManager.TYPE_ETHERNET;
        TConnectionType.Mobile:
          TypeTransport := TJConnectivityManager.TYPE_MOBILE;
        TConnectionType.Wifi:
          TypeTransport := TJConnectivityManager.TYPE_WIFI;
        TConnectionType.VPN:
          TypeTransport := TJConnectivityManager.TYPE_VPN;
      else
        TypeTransport := -1;
      end;

      NetworkInfo := GetNetworkInfo;
      if TypeTransport <> -1 then
        Result := (NetworkInfo <> nil) and (NetworkInfo.getType() = TypeTransport);
    end;
end;

class constructor TConnectionChecker.Create;
begin
  FConnectivityManager := nil;
  if CheckPermission then
    FConnectivityManager := TJConnectivityManager.Wrap
      (TfgAndroidHelper.Context.getSystemService(TJContext.CONNECTIVITY_SERVICE));
end;

class function TConnectionChecker.GetNetworkInfo: JNetworkInfo;
var
  NetworkInfo: JNetworkInfo;
begin
  Result := nil;
  if FConnectivityManager <> nil then
  begin
    NetworkInfo := FConnectivityManager.getActiveNetworkInfo();
    if NetworkInfo <> nil then
      Result := NetworkInfo;
  end;
end;

class function TConnectionChecker.GetNetworkCapabilities(const ANetwork: JNetwork): JNetworkCapabilities;
begin
  Result := nil;
  if TJBuild_VERSION.SDK_INT >= APILevel then
    if FConnectivityManager <> nil then
      if ANetwork <> nil then
        Result := FConnectivityManager.GetNetworkCapabilities(ANetwork);
end;

class function TConnectionChecker.IsConnected: Boolean;
var
  NetworkInfo: JNetworkInfo;
begin
  if TJBuild_VERSION.SDK_INT >= APILevel then
    Result := (FConnectivityManager <> nil) and (FConnectivityManager.getActiveNetwork <> nil)
  else
  begin
    NetworkInfo := GetNetworkInfo;
    Result := (NetworkInfo <> nil) and NetworkInfo.IsConnected();
  end;
end;

end.
