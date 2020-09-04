# FGX: Android API Examples - ConnectionChecker

### unit FGX.ConnectionChecker.Android:

 - **TConnectionType = (Ethernet, Mobile, Wifi);**
 
 - **class function CheckPermission: Boolean;** - Check permission "android.permission.ACCESS_NETWORK_STATE"
 - **class function IsConnected: Boolean;** - Indicates whether network connectivity exists and it is possible to establish connections and pass data.
 - **class function HasConnection(const AType: TConnectionType): Boolean;** - Check the connection type.
 - **class function HasInternet: Boolean;** - This is an experimental version of the internet check method. It is not stable. Android API Support >= 23