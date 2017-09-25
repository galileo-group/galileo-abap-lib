class /GAL/CONFIG_NODE_AUTHENTICATOR definition
  public
  abstract
  create public .

public section.
  type-pools ABAP .

  methods AUTHORITY_CHECK
  abstract
    importing
      !ORIGINAL_SYSTEM_ID type /GAL/SYSTEM_ID default SY-SYSID(3)
      !ORIGINAL_CLIENT type MANDT default SY-MANDT
      !ORIGINAL_USER type UNAME default SY-UNAME
      !NODE type ref to /GAL/CONFIG_NODE
      !ACTION type I
      !CLIENT type MANDT optional
      !USER_NAME type UNAME optional
    raising
      /GAL/CX_AUTH_CHECK_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS /GAL/CONFIG_NODE_AUTHENTICATOR IMPLEMENTATION.
ENDCLASS.
