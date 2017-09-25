*"* components of interface /GAL/IF_RFC_ROUTE_PROVIDER
interface /GAL/IF_RFC_ROUTE_PROVIDER
  public .


  methods GET_RFC_ROUTE_INFO
    importing
      !PURPOSE type /GAL/CFW_PURPOSE default `ANY`
      !TARGET_SYSTEM_ID type /GAL/SYSTEM_ID
      !TARGET_CLIENT_ID type MANDT default 'ANY'
    returning
      value(RFC_ROUTE_INFO) type /GAL/RFC_ROUTE_INFO
    raising
      /GAL/CX_CFW_EXCEPTION .
endinterface.
