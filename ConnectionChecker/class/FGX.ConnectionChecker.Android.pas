{ *********************************************************************
  *
  * Autor: Efimov A.A.
  * E-mail: infocean@gmail.com
  * GitHub: https://github.com/AndrewEfimov/FGX-Android-API-Examples
  * Description: Getting information about connecting.
  * Requirements: You need permission "ACCESS_NETWORK_STATE" in the manifest.
  * Platform: only Android (API 21 +) (Tested: API 22 and 27)
  * IDE (Tested): Delphi 10.3.3+ and FGX 1.4.5.0+
  *
  ******************************************************************** }
unit FGX.ConnectionChecker.Android;

interface

{$WARN SYMBOL_DEPRECATED OFF}
{$SCOPEDENUMS ON}

uses
  Android.Api.Network, FGX.Permissions, FGX.Helpers.Android, Android.Api.ActivityAndView;

type
  TConnectionType = (Ethernet, Mobile, Wifi);

  TConnectionChecker = class
  private const
    APILevel = 23;
  public const
    AccessNetworkStatePermission = 'android.permission.ACCESS_NETWORK_STATE';
  private
    class var FJConnectivityManager: JConnectivityManager;
    class constructor Create;
    /// <summary> Returns details about the currently active default data network. </summary>
    class function GetNetworkInfo: JNetworkInfo;
    /// <summary> Get the NetworkCapabilities for the given Network. </summary>
    class function GetNetworkCapabilities: JNetworkCapabilities;
  public
    /// <summary> Check permission "android.permission.ACCESS_NETWORK_STATE" </summary>
    class function CheckPermission: Boolean;
    /// <summary> Indicates whether network connectivity exists and it is possible to establish connections and pass data. </summary>
    class function IsConnected: Boolean;
    /// <summary> Check the connection type. </summary>
    class function HasConnection(const AType: TConnectionType): Boolean;
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
  NetworkCap: JNetworkCapabilities;
begin
  Result := False;
  if TJBuild_VERSION.SDK_INT >= APILevel then
  begin
    NetworkCap := GetNetworkCapabilities;
    Result := (NetworkCap <> nil) and NetworkCap.hasCapability(TJNetworkCapabilities.NET_CAPABILITY_INTERNET) and
      (NetworkCap.hasCapability(TJNetworkCapabilities.NET_CAPABILITY_VALIDATED));
  end;
end;

class function TConnectionChecker.CheckPermission: Boolean;
var
  PermissionInfo: TfgPermissionInfo;
begin
  PermissionInfo := TfgPermissionService.CheckPermission(AccessNetworkStatePermission);
  Result := PermissionInfo.CheckResult = TPermissionCheckResult.Granted;
end;

class function TConnectionChecker.HasConnection(const AType: TConnectionType): Boolean;
var
  NetworkCap: JNetworkCapabilities;
  TypeTransport: Integer;
  NetworkInfo: JNetworkInfo;
begin
  Result := False;
  if TJBuild_VERSION.SDK_INT >= APILevel then
  begin
    case AType of
      TConnectionType.Ethernet:
        TypeTransport := TJNetworkCapabilities.TRANSPORT_ETHERNET;
      TConnectionType.Mobile:
        TypeTransport := TJNetworkCapabilities.TRANSPORT_CELLULAR;
      TConnectionType.Wifi:
        TypeTransport := TJNetworkCapabilities.TRANSPORT_WIFI;
    else
      TypeTransport := -1;
    end;

    NetworkCap := GetNetworkCapabilities;
    if TypeTransport <> -1 then
      Result := (NetworkCap <> nil) and (NetworkCap.hasTransport(TypeTransport));
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
  FJConnectivityManager := nil;
  if CheckPermission then
    FJConnectivityManager := TJConnectivityManager.Wrap
      (TfgAndroidHelper.Context.getSystemService(TJContext.CONNECTIVITY_SERVICE));
end;

class function TConnectionChecker.GetNetworkInfo: JNetworkInfo;
var
  NetworkInfo: JNetworkInfo;
begin
  Result := nil;
  if FJConnectivityManager <> nil then
  begin
    NetworkInfo := FJConnectivityManager.getActiveNetworkInfo();
    if NetworkInfo <> nil then
      Result := NetworkInfo;
  end;
end;

class function TConnectionChecker.GetNetworkCapabilities: JNetworkCapabilities;
begin
  Result := nil;
  if TJBuild_VERSION.SDK_INT >= APILevel then
    if FJConnectivityManager <> nil then
      Result := FJConnectivityManager.GetNetworkCapabilities(FJConnectivityManager.getActiveNetwork);
end;

class function TConnectionChecker.IsConnected: Boolean;
var
  NetworkInfo: JNetworkInfo;
begin
  if TJBuild_VERSION.SDK_INT >= APILevel then
    Result := (FJConnectivityManager <> nil) and (FJConnectivityManager.getActiveNetwork <> nil)
  else
  begin
    NetworkInfo := GetNetworkInfo;
    Result := (NetworkInfo <> nil) and NetworkInfo.IsConnected();
  end;
end;

end.
