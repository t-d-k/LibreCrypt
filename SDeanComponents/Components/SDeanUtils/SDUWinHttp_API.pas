unit SDUWinHttp_API;
// Description: Sarah Dean's WinHttp port
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//
// This file contains a Delphi port of WinHttp.h


interface

uses
  Windows, SysUtils;

type
  EWinHttpException = Exception;


const INTERNET_DEFAULT_PORT           = 0;
const INTERNET_DEFAULT_HTTP_PORT      = 80;
const INTERNET_DEFAULT_HTTPS_PORT     = 443;

const INTERNET_SCHEME_HTTP            = 1;
const INTERNET_SCHEME_HTTPS           = 2;

const ICU_ESCAPE  = $80000000;

// Flags for WinHttpOpen
const WINHTTP_FLAG_ASYNC                  = $10000000;

// Flags for WinHttpOpenRequest 
const WINHTTP_FLAG_ESCAPE_PERCENT         = $00000004;
const WINHTTP_FLAG_nil_CODEPAGE           = $00000008;
const WINHTTP_FLAG_ESCAPE_DISABLE         = $00000040;
const WINHTTP_FLAG_ESCAPE_DISABLE_QUERY   = $00000080;
const WINHTTP_FLAG_BYPASS_PROXY_CACHE     = $00000100;
const WINHTTP_FLAG_REFRESH                = WINHTTP_FLAG_BYPASS_PROXY_CACHE;
const WINHTTP_FLAG_SECURE                 = $00800000;

const WINHTTP_ACCESS_TYPE_DEFAULT_PROXY   = 0;
const WINHTTP_ACCESS_TYPE_NO_PROXY        = 1;
const WINHTTP_ACCESS_TYPE_NAMED_PROXY     = 3;

const WINHTTP_NO_PROXY_NAME               = nil;
const WINHTTP_NO_PROXY_BYPASS             = nil;

const WINHTTP_NO_REFERER                  = nil;
const WINHTTP_DEFAULT_ACCEPT_TYPES        = nil;

const WINHTTP_NO_ADDITIONAL_HEADERS       = nil;
const WINHTTP_NO_REQUEST_DATA             = nil;

const WINHTTP_HEADER_NAME_BY_INDEX        = nil;
const WINHTTP_NO_OUTPUT_BUFFER            = nil;
const WINHTTP_NO_HEADER_INDEX             = nil;

const WINHTTP_ADDREQ_INDEX_MASK                    = $0000FFFF;
const WINHTTP_ADDREQ_FLAGS_MASK                    = $FFFF0000;
const WINHTTP_ADDREQ_FLAG_ADD_IF_NEW               = $10000000;
const WINHTTP_ADDREQ_FLAG_ADD                      = $20000000;
const WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA      = $40000000;
const WINHTTP_ADDREQ_FLAG_COALESCE_WITH_SEMICOLON  = $01000000;
const WINHTTP_ADDREQ_FLAG_COALESCE                 = WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA;
const WINHTTP_ADDREQ_FLAG_REPLACE                  = $80000000;

const WINHTTP_IGNORE_REQUEST_TOTAL_LENGTH = 0;

// Flags for WinHttp{Set/Query}Options
//const WINHTTP_FIRST_OPTION                         = 1;
const WINHTTP_OPTION_CALLBACK                      = 1;
const WINHTTP_OPTION_RESOLVE_TIMEOUT               = 2;
const WINHTTP_OPTION_CONNECT_TIMEOUT               = 3;
const WINHTTP_OPTION_CONNECT_RETRIES               = 4;
const WINHTTP_OPTION_SEND_TIMEOUT                  = 5;
const WINHTTP_OPTION_RECEIVE_TIMEOUT               = 6;
const WINHTTP_OPTION_RECEIVE_RESPONSE_TIMEOUT      = 7;
const WINHTTP_OPTION_HANDLE_TYPE                   = 9;
const WINHTTP_OPTION_READ_BUFFER_SIZE              = 12;
const WINHTTP_OPTION_WRITE_BUFFER_SIZE             = 13;
const WINHTTP_OPTION_PARENT_HANDLE                 = 21;
const WINHTTP_OPTION_EXTENDED_ERROR                = 24;
const WINHTTP_OPTION_SECURITY_FLAGS                = 31;
const WINHTTP_OPTION_SECURITY_CERTIFICATE_STRUCT   = 32;
const WINHTTP_OPTION_URL                           = 34;
const WINHTTP_OPTION_SECURITY_KEY_BITNESS          = 36;
const WINHTTP_OPTION_PROXY                         = 38;
const WINHTTP_OPTION_USER_AGENT                    = 41;
const WINHTTP_OPTION_CONTEXT_VALUE                 = 45;
const WINHTTP_OPTION_CLIENT_CERT_CONTEXT           = 47;
const WINHTTP_OPTION_REQUEST_PRIORITY              = 58;
const WINHTTP_OPTION_HTTP_VERSION                  = 59;
const WINHTTP_OPTION_DISABLE_FEATURE               = 63;
const WINHTTP_OPTION_CODEPAGE                      = 68;
const WINHTTP_OPTION_MAX_CONNS_PER_SERVER          = 73;
const WINHTTP_OPTION_MAX_CONNS_PER_1_0_SERVER      = 74;
const WINHTTP_OPTION_AUTOLOGON_POLICY              = 77;
const WINHTTP_OPTION_SERVER_CERT_CONTEXT           = 78;
const WINHTTP_OPTION_ENABLE_FEATURE                = 79;
const WINHTTP_OPTION_WORKER_THREAD_COUNT           = 80;
const WINHTTP_OPTION_PASSPORT_COBRANDING_TEXT      = 81;
const WINHTTP_OPTION_PASSPORT_COBRANDING_URL       = 82;
const WINHTTP_OPTION_CONFIGURE_PASSPORT_AUTH       = 83;
const WINHTTP_OPTION_SECURE_PROTOCOLS              = 84;
const WINHTTP_OPTION_ENABLETRACING                 = 85;
const WINHTTP_OPTION_PASSPORT_SIGN_OUT             = 86;
const WINHTTP_OPTION_PASSPORT_RETURN_URL           = 87;
const WINHTTP_OPTION_REDIRECT_POLICY               = 88;
const WINHTTP_OPTION_MAX_HTTP_AUTOMATIC_REDIRECTS  = 89;
const WINHTTP_OPTION_MAX_HTTP_STATUS_CONTINUE      = 90;
const WINHTTP_OPTION_MAX_RESPONSE_HEADER_SIZE      = 91;
const WINHTTP_OPTION_MAX_RESPONSE_DRAIN_SIZE       = 92;
const WINHTTP_OPTION_CONNECTION_INFO               = 93;
const WINHTTP_OPTION_CLIENT_CERT_ISSUER_LIST       = 94;
const WINHTTP_OPTION_SPN                           = 96;
const WINHTTP_OPTION_GLOBAL_PROXY_CREDS            = 97;
const WINHTTP_OPTION_GLOBAL_SERVER_CREDS           = 98;
const WINHTTP_OPTION_UNLOAD_NOTIFY_EVENT           = 99;
const WINHTTP_OPTION_REJECT_USERPWD_IN_URL         = 100;
const WINHTTP_OPTION_USE_GLOBAL_SERVER_CREDENTIALS = 101;
//const WINHTTP_LAST_OPTION                          = WINHTTP_OPTION_USE_GLOBAL_SERVER_CREDENTIALS;
const WINHTTP_OPTION_USERNAME                      = $1000;
const WINHTTP_OPTION_PASSWORD                      = $1001;
const WINHTTP_OPTION_PROXY_USERNAME                = $1002;
const WINHTTP_OPTION_PROXY_PASSWORD                = $1003;

const WINHTTP_CONNS_PER_SERVER_UNLIMITED = $FFFFFFFF;

const WINHTTP_AUTOLOGON_SECURITY_LEVEL_MEDIUM   = 0;
const WINHTTP_AUTOLOGON_SECURITY_LEVEL_LOW      = 1;
const WINHTTP_AUTOLOGON_SECURITY_LEVEL_HIGH     = 2;
const WINHTTP_AUTOLOGON_SECURITY_LEVEL_DEFAULT  = WINHTTP_AUTOLOGON_SECURITY_LEVEL_MEDIUM;

const WINHTTP_OPTION_REDIRECT_POLICY_NEVER                        = 0;
const WINHTTP_OPTION_REDIRECT_POLICY_DISALLOW_HTTPS_TO_HTTP       = 1;
const WINHTTP_OPTION_REDIRECT_POLICY_ALWAYS                       = 2;
const WINHTTP_OPTION_REDIRECT_POLICY_LAST            = WINHTTP_OPTION_REDIRECT_POLICY_ALWAYS;
const WINHTTP_OPTION_REDIRECT_POLICY_DEFAULT         = WINHTTP_OPTION_REDIRECT_POLICY_DISALLOW_HTTPS_TO_HTTP;

const WINHTTP_DISABLE_PASSPORT_AUTH    = $00000000;
const WINHTTP_ENABLE_PASSPORT_AUTH     = $10000000;
const WINHTTP_DISABLE_PASSPORT_KEYRING = $20000000;
const WINHTTP_ENABLE_PASSPORT_KEYRING  = $40000000;

const WINHTTP_DISABLE_COOKIES                   = $00000001;
const WINHTTP_DISABLE_REDIRECTS                 = $00000002;
const WINHTTP_DISABLE_AUTHENTICATION            = $00000004;
const WINHTTP_DISABLE_KEEP_ALIVE                = $00000008;
const WINHTTP_ENABLE_SSL_REVOCATION             = $00000001;
const WINHTTP_ENABLE_SSL_REVERT_IMPERSONATION   = $00000002;
const WINHTTP_DISABLE_SPN_SERVER_PORT           = $00000000;
const WINHTTP_ENABLE_SPN_SERVER_PORT            = $00000001;
const WINHTTP_OPTION_SPN_MASK                   = WINHTTP_ENABLE_SPN_SERVER_PORT;

// WinHTTP error codes
const WINHTTP_ERROR_BASE                                  = 12000;
const ERROR_WINHTTP_OUT_OF_HANDLES                        = (WINHTTP_ERROR_BASE + 1);
const ERROR_WINHTTP_TIMEOUT                               = (WINHTTP_ERROR_BASE + 2);
const ERROR_WINHTTP_INTERNAL_ERROR                        = (WINHTTP_ERROR_BASE + 4);
const ERROR_WINHTTP_INVALID_URL                           = (WINHTTP_ERROR_BASE + 5);
const ERROR_WINHTTP_UNRECOGNIZED_SCHEME                   = (WINHTTP_ERROR_BASE + 6);
const ERROR_WINHTTP_NAME_NOT_RESOLVED                     = (WINHTTP_ERROR_BASE + 7);
const ERROR_WINHTTP_INVALID_OPTION                        = (WINHTTP_ERROR_BASE + 9);
const ERROR_WINHTTP_OPTION_NOT_SETTABLE                   = (WINHTTP_ERROR_BASE + 11);
const ERROR_WINHTTP_SHUTDOWN                              = (WINHTTP_ERROR_BASE + 12);
const ERROR_WINHTTP_LOGIN_FAILURE                         = (WINHTTP_ERROR_BASE + 15);
const ERROR_WINHTTP_OPERATION_CANCELLED                   = (WINHTTP_ERROR_BASE + 17);
const ERROR_WINHTTP_INCORRECT_HANDLE_TYPE                 = (WINHTTP_ERROR_BASE + 18);
const ERROR_WINHTTP_INCORRECT_HANDLE_STATE                = (WINHTTP_ERROR_BASE + 19);
const ERROR_WINHTTP_CANNOT_CONNECT                        = (WINHTTP_ERROR_BASE + 29);
const ERROR_WINHTTP_CONNECTION_ERROR                      = (WINHTTP_ERROR_BASE + 30);
const ERROR_WINHTTP_RESEND_REQUEST                        = (WINHTTP_ERROR_BASE + 32);
const ERROR_WINHTTP_SECURE_CERT_DATE_INVALID              = (WINHTTP_ERROR_BASE + 37);
const ERROR_WINHTTP_SECURE_CERT_CN_INVALID                = (WINHTTP_ERROR_BASE + 38);
const ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED               = (WINHTTP_ERROR_BASE + 44);
const ERROR_WINHTTP_SECURE_INVALID_CA                     = (WINHTTP_ERROR_BASE + 45);
const ERROR_WINHTTP_SECURE_CERT_REV_FAILED                = (WINHTTP_ERROR_BASE + 57);
const ERROR_WINHTTP_CANNOT_CALL_BEFORE_OPEN               = (WINHTTP_ERROR_BASE + 100);
const ERROR_WINHTTP_CANNOT_CALL_BEFORE_SEND               = (WINHTTP_ERROR_BASE + 101);
const ERROR_WINHTTP_CANNOT_CALL_AFTER_SEND                = (WINHTTP_ERROR_BASE + 102);
const ERROR_WINHTTP_CANNOT_CALL_AFTER_OPEN                = (WINHTTP_ERROR_BASE + 103);
const ERROR_WINHTTP_HEADER_NOT_FOUND                      = (WINHTTP_ERROR_BASE + 150);
const ERROR_WINHTTP_INVALID_SERVER_RESPONSE               = (WINHTTP_ERROR_BASE + 152);
const ERROR_WINHTTP_INVALID_HEADER                        = (WINHTTP_ERROR_BASE + 153);
const ERROR_WINHTTP_INVALID_QUERY_REQUEST                 = (WINHTTP_ERROR_BASE + 154);
const ERROR_WINHTTP_HEADER_ALREADY_EXISTS                 = (WINHTTP_ERROR_BASE + 155);
const ERROR_WINHTTP_REDIRECT_FAILED                       = (WINHTTP_ERROR_BASE + 156);
const ERROR_WINHTTP_SECURE_CHANNEL_ERROR                  = (WINHTTP_ERROR_BASE + 157);
const ERROR_WINHTTP_BAD_AUTO_PROXY_SCRIPT                 = (WINHTTP_ERROR_BASE + 166);
const ERROR_WINHTTP_UNABLE_TO_DOWNLOAD_SCRIPT             = (WINHTTP_ERROR_BASE + 167);
const ERROR_WINHTTP_SECURE_INVALID_CERT                   = (WINHTTP_ERROR_BASE + 169);
const ERROR_WINHTTP_SECURE_CERT_REVOKED                   = (WINHTTP_ERROR_BASE + 170);
const ERROR_WINHTTP_NOT_INITIALIZED                       = (WINHTTP_ERROR_BASE + 172);
const ERROR_WINHTTP_SECURE_FAILURE                        = (WINHTTP_ERROR_BASE + 175);
const ERROR_WINHTTP_AUTO_PROXY_SERVICE_ERROR              = (WINHTTP_ERROR_BASE + 178);
const ERROR_WINHTTP_SECURE_CERT_WRONG_USAGE               = (WINHTTP_ERROR_BASE + 179);
const ERROR_WINHTTP_AUTODETECTION_FAILED                  = (WINHTTP_ERROR_BASE + 180);
const ERROR_WINHTTP_HEADER_COUNT_EXCEEDED                 = (WINHTTP_ERROR_BASE + 181);
const ERROR_WINHTTP_HEADER_SIZE_OVERFLOW                  = (WINHTTP_ERROR_BASE + 182);
const ERROR_WINHTTP_CHUNKED_ENCODING_HEADER_SIZE_OVERFLOW = (WINHTTP_ERROR_BASE + 183);
const ERROR_WINHTTP_RESPONSE_DRAIN_OVERFLOW               = (WINHTTP_ERROR_BASE + 184);
const ERROR_WINHTTP_CLIENT_CERT_NO_PRIVATE_KEY            = (WINHTTP_ERROR_BASE + 185);
const ERROR_WINHTTP_CLIENT_CERT_NO_ACCESS_PRIVATE_KEY     = (WINHTTP_ERROR_BASE + 186);
const WINHTTP_ERROR_LAST                                  = (WINHTTP_ERROR_BASE + 186);

// WinHttp status codes
const HTTP_STATUS_CONTINUE            = 100;
const HTTP_STATUS_SWITCH_PROTOCOLS    = 101;
const HTTP_STATUS_OK                  = 200;
const HTTP_STATUS_CREATED             = 201;
const HTTP_STATUS_ACCEPTED            = 202;
const HTTP_STATUS_PARTIAL             = 203;
const HTTP_STATUS_NO_CONTENT          = 204;
const HTTP_STATUS_RESET_CONTENT       = 205;
const HTTP_STATUS_PARTIAL_CONTENT     = 206;
const HTTP_STATUS_WEBDAV_MULTI_STATUS = 207;
const HTTP_STATUS_AMBIGUOUS           = 300;
const HTTP_STATUS_MOVED               = 301;
const HTTP_STATUS_REDIRECT            = 302;
const HTTP_STATUS_REDIRECT_METHOD     = 303;
const HTTP_STATUS_NOT_MODIFIED        = 304;
const HTTP_STATUS_USE_PROXY           = 305;
const HTTP_STATUS_REDIRECT_KEEP_VERB  = 307;
const HTTP_STATUS_BAD_REQUEST         = 400;
const HTTP_STATUS_DENIED              = 401;
const HTTP_STATUS_PAYMENT_REQ         = 402;
const HTTP_STATUS_FORBIDDEN           = 403;
const HTTP_STATUS_NOT_FOUND           = 404;
const HTTP_STATUS_BAD_METHOD          = 405;
const HTTP_STATUS_NONE_ACCEPTABLE     = 406;
const HTTP_STATUS_PROXY_AUTH_REQ      = 407;
const HTTP_STATUS_REQUEST_TIMEOUT     = 408;
const HTTP_STATUS_CONFLICT            = 409;
const HTTP_STATUS_GONE                = 410;
const HTTP_STATUS_LENGTH_REQUIRED     = 411;
const HTTP_STATUS_PRECOND_FAILED      = 412;
const HTTP_STATUS_REQUEST_TOO_LARGE   = 413;
const HTTP_STATUS_URI_TOO_LONG        = 414;
const HTTP_STATUS_UNSUPPORTED_MEDIA   = 415;
const HTTP_STATUS_RETRY_WITH          = 449;
const HTTP_STATUS_SERVER_ERROR        = 500;
const HTTP_STATUS_NOT_SUPPORTED       = 501;
const HTTP_STATUS_BAD_GATEWAY         = 502;
const HTTP_STATUS_SERVICE_UNAVAIL     = 503;
const HTTP_STATUS_GATEWAY_TIMEOUT     = 504;
const HTTP_STATUS_VERSION_NOT_SUP     = 505;
const HTTP_STATUS_FIRST               = HTTP_STATUS_CONTINUE;
const HTTP_STATUS_LAST                = HTTP_STATUS_VERSION_NOT_SUP;

const SECURITY_FLAG_IGNORE_UNKNOWN_CA         = $00000100;
const SECURITY_FLAG_IGNORE_CERT_DATE_INVALID  = $00002000;
const SECURITY_FLAG_IGNORE_CERT_CN_INVALID    = $00001000;
const SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE   = $00000200;
const SECURITY_FLAG_SECURE                    = $00000001;
const SECURITY_FLAG_STRENGTH_WEAK             = $10000000;
const SECURITY_FLAG_STRENGTH_MEDIUM           = $40000000;
const SECURITY_FLAG_STRENGTH_STRONG           = $20000000;

const ICU_NO_ENCODE          = $20000000;
const ICU_DECODE             = $10000000;
const ICU_NO_META            = $08000000;
const ICU_ENCODE_SPACES_ONLY = $04000000;
const ICU_BROWSER_MODE       = $02000000;
const ICU_ENCODE_PERCENT     = $00001000;

// Query flags
const WINHTTP_QUERY_MIME_VERSION                 = 0;
const WINHTTP_QUERY_CONTENT_TYPE                 = 1;
const WINHTTP_QUERY_CONTENT_TRANSFER_ENCODING    = 2;
const WINHTTP_QUERY_CONTENT_ID                   = 3;
const WINHTTP_QUERY_CONTENT_DESCRIPTION          = 4;
const WINHTTP_QUERY_CONTENT_LENGTH               = 5;
const WINHTTP_QUERY_CONTENT_LANGUAGE             = 6;
const WINHTTP_QUERY_ALLOW                        = 7;
const WINHTTP_QUERY_PUBLIC                       = 8;
const WINHTTP_QUERY_DATE                         = 9;
const WINHTTP_QUERY_EXPIRES                      = 10;
const WINHTTP_QUERY_LAST_MODIFIED                = 11;
const WINHTTP_QUERY_MESSAGE_ID                   = 12;
const WINHTTP_QUERY_URI                          = 13;
const WINHTTP_QUERY_DERIVED_FROM                 = 14;
const WINHTTP_QUERY_COST                         = 15;
const WINHTTP_QUERY_LINK                         = 16;
const WINHTTP_QUERY_PRAGMA                       = 17;
const WINHTTP_QUERY_VERSION                      = 18;
const WINHTTP_QUERY_STATUS_CODE                  = 19;
const WINHTTP_QUERY_STATUS_TEXT                  = 20;
const WINHTTP_QUERY_RAW_HEADERS                  = 21;
const WINHTTP_QUERY_RAW_HEADERS_CRLF             = 22;
const WINHTTP_QUERY_CONNECTION                   = 23;
const WINHTTP_QUERY_ACCEPT                       = 24;
const WINHTTP_QUERY_ACCEPT_CHARSET               = 25;
const WINHTTP_QUERY_ACCEPT_ENCODING              = 26;
const WINHTTP_QUERY_ACCEPT_LANGUAGE              = 27;
const WINHTTP_QUERY_AUTHORIZATION                = 28;
const WINHTTP_QUERY_CONTENT_ENCODING             = 29;
const WINHTTP_QUERY_FORWARDED                    = 30;
const WINHTTP_QUERY_FROM                         = 31;
const WINHTTP_QUERY_IF_MODIFIED_SINCE            = 32;
const WINHTTP_QUERY_LOCATION                     = 33;
const WINHTTP_QUERY_ORIG_URI                     = 34;
const WINHTTP_QUERY_REFERER                      = 35;
const WINHTTP_QUERY_RETRY_AFTER                  = 36;
const WINHTTP_QUERY_SERVER                       = 37;
const WINHTTP_QUERY_TITLE                        = 38;
const WINHTTP_QUERY_USER_AGENT                   = 39;
const WINHTTP_QUERY_WWW_AUTHENTICATE             = 40;
const WINHTTP_QUERY_PROXY_AUTHENTICATE           = 41;
const WINHTTP_QUERY_ACCEPT_RANGES                = 42;
const WINHTTP_QUERY_SET_COOKIE                   = 43;
const WINHTTP_QUERY_COOKIE                       = 44;
const WINHTTP_QUERY_REQUEST_METHOD               = 45;
const WINHTTP_QUERY_REFRESH                      = 46;
const WINHTTP_QUERY_CONTENT_DISPOSITION          = 47;
const WINHTTP_QUERY_AGE                          = 48;
const WINHTTP_QUERY_CACHE_CONTROL                = 49;
const WINHTTP_QUERY_CONTENT_BASE                 = 50;
const WINHTTP_QUERY_CONTENT_LOCATION             = 51;
const WINHTTP_QUERY_CONTENT_MD5                  = 52;
const WINHTTP_QUERY_CONTENT_RANGE                = 53;
const WINHTTP_QUERY_ETAG                         = 54;
const WINHTTP_QUERY_HOST                         = 55;
const WINHTTP_QUERY_IF_MATCH                     = 56;
const WINHTTP_QUERY_IF_NONE_MATCH                = 57;
const WINHTTP_QUERY_IF_RANGE                     = 58;
const WINHTTP_QUERY_IF_UNMODIFIED_SINCE          = 59;
const WINHTTP_QUERY_MAX_FORWARDS                 = 60;
const WINHTTP_QUERY_PROXY_AUTHORIZATION          = 61;
const WINHTTP_QUERY_RANGE                        = 62;
const WINHTTP_QUERY_TRANSFER_ENCODING            = 63;
const WINHTTP_QUERY_UPGRADE                      = 64;
const WINHTTP_QUERY_VARY                         = 65;
const WINHTTP_QUERY_VIA                          = 66;
const WINHTTP_QUERY_WARNING                      = 67;
const WINHTTP_QUERY_EXPECT                       = 68;
const WINHTTP_QUERY_PROXY_CONNECTION             = 69;
const WINHTTP_QUERY_UNLESS_MODIFIED_SINCE        = 70;
const WINHTTP_QUERY_PROXY_SUPPORT                = 75;
const WINHTTP_QUERY_AUTHENTICATION_INFO          = 76;
const WINHTTP_QUERY_PASSPORT_URLS                = 77;
const WINHTTP_QUERY_PASSPORT_CONFIG              = 78;
const WINHTTP_QUERY_MAX                          = 78;
const WINHTTP_QUERY_CUSTOM                       = 65535;
const WINHTTP_QUERY_FLAG_REQUEST_HEADERS         = $80000000;
const WINHTTP_QUERY_FLAG_SYSTEMTIME              = $40000000;
const WINHTTP_QUERY_FLAG_NUMBER                  = $20000000;

// Callback options
const WINHTTP_CALLBACK_STATUS_RESOLVING_NAME          = $00000001;
const WINHTTP_CALLBACK_STATUS_NAME_RESOLVED           = $00000002;
const WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER    = $00000004;
const WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER     = $00000008;
const WINHTTP_CALLBACK_STATUS_SENDING_REQUEST         = $00000010;
const WINHTTP_CALLBACK_STATUS_REQUEST_SENT            = $00000020;
const WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE      = $00000040;
const WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED       = $00000080;
const WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION      = $00000100;
const WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED       = $00000200;
const WINHTTP_CALLBACK_STATUS_HANDLE_CREATED          = $00000400;
const WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING          = $00000800;
const WINHTTP_CALLBACK_STATUS_DETECTING_PROXY         = $00001000;
const WINHTTP_CALLBACK_STATUS_REDIRECT                = $00004000;
const WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE   = $00008000;
const WINHTTP_CALLBACK_STATUS_SECURE_FAILURE          = $00010000;
const WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE       = $00020000;
const WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE          = $00040000;
const WINHTTP_CALLBACK_STATUS_READ_COMPLETE           = $00080000;
const WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE          = $00100000;
const WINHTTP_CALLBACK_STATUS_REQUEST_ERROR           = $00200000;
const WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE    = $00400000;
const WINHTTP_CALLBACK_FLAG_RESOLVE_NAME              = (WINHTTP_CALLBACK_STATUS_RESOLVING_NAME or WINHTTP_CALLBACK_STATUS_NAME_RESOLVED);
const WINHTTP_CALLBACK_FLAG_CONNECT_TO_SERVER         = (WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER or WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER);
const WINHTTP_CALLBACK_FLAG_SEND_REQUEST              = (WINHTTP_CALLBACK_STATUS_SENDING_REQUEST or WINHTTP_CALLBACK_STATUS_REQUEST_SENT);
const WINHTTP_CALLBACK_FLAG_RECEIVE_RESPONSE          = (WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE or WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED);
const WINHTTP_CALLBACK_FLAG_CLOSE_CONNECTION          = (WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION or WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED);
const WINHTTP_CALLBACK_FLAG_HANDLES                   = (WINHTTP_CALLBACK_STATUS_HANDLE_CREATED or WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING);
const WINHTTP_CALLBACK_FLAG_DETECTING_PROXY           = WINHTTP_CALLBACK_STATUS_DETECTING_PROXY;
const WINHTTP_CALLBACK_FLAG_REDIRECT                  = WINHTTP_CALLBACK_STATUS_REDIRECT;
const WINHTTP_CALLBACK_FLAG_INTERMEDIATE_RESPONSE     = WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE;
const WINHTTP_CALLBACK_FLAG_SECURE_FAILURE            = WINHTTP_CALLBACK_STATUS_SECURE_FAILURE;
const WINHTTP_CALLBACK_FLAG_SENDREQUEST_COMPLETE      = WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE;
const WINHTTP_CALLBACK_FLAG_HEADERS_AVAILABLE         = WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE;
const WINHTTP_CALLBACK_FLAG_DATA_AVAILABLE            = WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE;
const WINHTTP_CALLBACK_FLAG_READ_COMPLETE             = WINHTTP_CALLBACK_STATUS_READ_COMPLETE;
const WINHTTP_CALLBACK_FLAG_WRITE_COMPLETE            = WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE;
const WINHTTP_CALLBACK_FLAG_REQUEST_ERROR             = WINHTTP_CALLBACK_STATUS_REQUEST_ERROR;
const WINHTTP_CALLBACK_FLAG_ALL_COMPLETIONS           = (WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE or WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE
                                                        or WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE or WINHTTP_CALLBACK_STATUS_READ_COMPLETE
                                                        or WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE or WINHTTP_CALLBACK_STATUS_REQUEST_ERROR);
const WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS         = $ffffffff;
//const WINHTTP_INVALID_STATUS_CALLBACK                 = ((WINHTTP_STATUS_CALLBACK)(-1L));
const WINHTTP_INVALID_STATUS_CALLBACK                 = -1;

const API_RECEIVE_RESPONSE          = (1);
const API_QUERY_DATA_AVAILABLE      = (2);
const API_READ_DATA                 = (3);
const API_WRITE_DATA                = (4);
const API_SEND_REQUEST              = (5);

const WINHTTP_HANDLE_TYPE_SESSION                  = 1;
const WINHTTP_HANDLE_TYPE_CONNECT                  = 2;
const WINHTTP_HANDLE_TYPE_REQUEST                  = 3;

const WINHTTP_CALLBACK_STATUS_FLAG_CERT_REV_FAILED         = $00000001;
const WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CERT            = $00000002;
const WINHTTP_CALLBACK_STATUS_FLAG_CERT_REVOKED            = $00000004;
const WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CA              = $00000008;
const WINHTTP_CALLBACK_STATUS_FLAG_CERT_CN_INVALID         = $00000010;
const WINHTTP_CALLBACK_STATUS_FLAG_CERT_DATE_INVALID       = $00000020;
const WINHTTP_CALLBACK_STATUS_FLAG_CERT_WRONG_USAGE        = $00000040;
const WINHTTP_CALLBACK_STATUS_FLAG_SECURITY_CHANNEL_ERROR  = $80000000;

const WINHTTP_AUTH_SCHEME_BASIC      = $00000001;
const WINHTTP_AUTH_SCHEME_NTLM       = $00000002;
const WINHTTP_AUTH_SCHEME_PASSPORT   = $00000004;
const WINHTTP_AUTH_SCHEME_DIGEST     = $00000008;
const WINHTTP_AUTH_SCHEME_NEGOTIATE  = $00000010;

const WINHTTP_AUTH_TARGET_SERVER     = $00000000;
const WINHTTP_AUTH_TARGET_PROXY      = $00000001;

const WINHTTP_TIME_FORMAT_BUFSIZE    = 62;


const NAME_RESOLUTION_TIMEOUT         = 15000;
const WINHTTP_DEFAULT_TIMEOUT_RESOLVE = 0;
const WINHTTP_DEFAULT_TIMEOUT_CONNECT = 60000;
const WINHTTP_DEFAULT_TIMEOUT_SEND    = 30000;
const WINHTTP_DEFAULT_TIMEOUT_RECEIVE = 30000;


type
  HINTERNET = THandle;
  LPCWSTR = PWideString;
  INTERNET_SCHEME = integer;
  INTERNET_PORT = integer; // WORD;  URL_COMPONENTS uses integer
  DWORD_PTR = PDWORD;
  LPVOID = Pointer;
  PLPCWSTR = ^LPCWSTR;
  int = integer;
  LPCVOID = Pointer;

URL_COMPONENTS = packed record
    dwStructSize: DWORD;
    lpszScheme: LPWSTR;
    dwSchemeLength: DWORD;
    nScheme: INTERNET_SCHEME;
    lpszHostName: LPWSTR;
    dwHostNameLength: DWORD;
    nPort: INTERNET_PORT;
    lpszUserName: LPWSTR;
    dwUserNameLength: DWORD;
    lpszPassword: LPWSTR;
    dwPasswordLength: DWORD;
    lpszUrlPath: LPWSTR;
    dwUrlPathLength: DWORD;
    lpszExtraInfo: LPWSTR;
    dwExtraInfoLength: DWORD;
end;
PURL_COMPONENTS = ^URL_COMPONENTS;
LPURL_COMPONENTS = PURL_COMPONENTS;
URL_COMPONENTSW = URL_COMPONENTS;
LPURL_COMPONENTSW = LPURL_COMPONENTS;

WINHTTP_ASYNC_RESULT = packed record
    dwResult: DWORD_PTR;
    dwError: DWORD;
end;
PWINHTTP_ASYNC_RESULT = ^WINHTTP_ASYNC_RESULT;
LPWINHTTP_ASYNC_RESULT = PWINHTTP_ASYNC_RESULT;

WINHTTP_CERTIFICATE_INFO = packed record
    ftExpiry: FILETIME;
    ftStart: FILETIME;
    lpszSubjectInfo: LPWSTR;
    lpszIssuerInfo: LPWSTR;
    lpszProtocolName: LPWSTR;
    lpszSignatureAlgName: LPWSTR;
    lpszEncryptionAlgName: LPWSTR;
    dwKeySize: DWORD;
end;

WINHTTP_PROXY_INFO = packed record
    dwAccessType: DWORD;
    lpszProxy: LPCWSTR;
    lpszProxyBypass: LPCWSTR;
end;
PWINHTTP_PROXY_INFO = ^WINHTTP_PROXY_INFO;
LPWINHTTP_PROXY_INFO = PWINHTTP_PROXY_INFO;
WINHTTP_PROXY_INFOW = WINHTTP_PROXY_INFO;
LPWINHTTP_PROXY_INFOW = LPWINHTTP_PROXY_INFO;

WINHTTP_CURRENT_USER_IE_PROXY_CONFIG = packed record
    fAutoDetect: BOOL;
    lpszAutoConfigUrl: LPWSTR;
    lpszProxy: LPWSTR;
    lpszProxyBypass: LPWSTR;
end;
PWINHTTP_CURRENT_USER_IE_PROXY_CONFIG = ^WINHTTP_CURRENT_USER_IE_PROXY_CONFIG;

//typedef VOID (CALLBACK *WINHTTP_STATUS_CALLBACK)(HINTERNET,DWORD_PTR,DWORD,LPVOID,DWORD);
WINHTTP_STATUS_CALLBACK = procedure (hIntenet: HINTERNET; a: DWORD_PTR; b: DWORD; c: LPVOID; d: DWORD);

WINHTTP_AUTOPROXY_OPTIONS = packed record
    dwFlags: DWORD;
    dwAutoDetectFlags: DWORD;
    lpszAutoConfigUrl: LPCWSTR;
    lpvReserved: LPVOID;
    dwReserved: DWORD;
    fAutoLogonIfChallenged: BOOL;
end;
PWINHTTP_AUTOPROXY_OPTIONS = ^WINHTTP_AUTOPROXY_OPTIONS;

HTTP_VERSION_INFO = packed record
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
end;
PHTTP_VERSION_INFO = ^HTTP_VERSION_INFO;
LPHTTP_VERSION_INFO = PHTTP_VERSION_INFO;


function WinHttpAddRequestHeaders(
  hRequest: HINTERNET;
  pwszHeaders: LPCWSTR;
  dwHeadersLength: DWORD;
  dwModifiers: DWORD
): BOOL; stdcall;
{$EXTERNALSYM WinHttpAddRequestHeaders}
function WinHttpCheckPlatform(): BOOL; stdcall;
{$EXTERNALSYM WinHttpCheckPlatform}
function WinHttpCloseHandle(
  hInternet: HINTERNET
): BOOL; stdcall;
{$EXTERNALSYM WinHttpCloseHandle}
function WinHttpConnect(
  hSession: HINTERNET;
  pswzServerName: LPCWSTR;
  nServerPort: INTERNET_PORT;
  dwReserved: DWORD
): HINTERNET; stdcall;
{$EXTERNALSYM WinHttpConnect}
function WinHttpCrackUrl(
  pwszUrl: LPCWSTR;
  dwUrlLength: DWORD;
  dwFlags: DWORD;
  lpUrlComponents: LPURL_COMPONENTS
): BOOL; stdcall;
{$EXTERNALSYM WinHttpCrackUrl}
function WinHttpCreateUrl(
  lpUrlComponents: LPURL_COMPONENTS;
  dwFlags: DWORD;
  pwszUrl: LPWSTR;
  lpdwUrlLength: LPDWORD
): BOOL; stdcall;
{$EXTERNALSYM WinHttpCreateUrl}
function WinHttpDetectAutoProxyConfigUrl(
  dwAutoDetectFlags: DWORD;
  ppwszAutoConfigUrl: PLPWSTR
): BOOL; stdcall;
{$EXTERNALSYM WinHttpDetectAutoProxyConfigUrl}
function WinHttpGetDefaultProxyConfiguration(
  pProxyInfo: PWINHTTP_PROXY_INFO
): BOOL; stdcall;
{$EXTERNALSYM WinHttpGetDefaultProxyConfiguration}
function WinHttpGetIEProxyConfigForCurrentUser(
  pProxyConfig: PWINHTTP_CURRENT_USER_IE_PROXY_CONFIG
): BOOL; stdcall;
{$EXTERNALSYM WinHttpGetIEProxyConfigForCurrentUser}
function WinHttpGetProxyForUrl(
  hSession: HINTERNET;
  lpcwszUrl: LPCWSTR;
  pAutoProxyOptions: PWINHTTP_AUTOPROXY_OPTIONS;
  pProxyInfo: PWINHTTP_PROXY_INFO
): BOOL; stdcall;
{$EXTERNALSYM WinHttpGetProxyForUrl}
function WinHttpOpen(
  pwszUserAgent: LPCWSTR;
  dwAccessType: DWORD;
  pwszProxyName: LPCWSTR;
  pwszProxyBypass: LPCWSTR;
  dwFlags: DWORD
): HINTERNET; stdcall;
{$EXTERNALSYM WinHttpOpen}
function WinHttpOpenRequest(
  hConnect: HINTERNET;
  pwszVerb: LPCWSTR;
  pwszObjectName: LPCWSTR;
  pwszVersion: LPCWSTR;
  pwszReferrer: LPCWSTR;
  ppwszAcceptTypes: PLPCWSTR;
  dwFlags: DWORD
): HINTERNET; stdcall;
{$EXTERNALSYM WinHttpOpenRequest}
function WinHttpQueryAuthSchemes(
  hRequest: HINTERNET;
  lpdwSupportedSchemes: LPDWORD;
  lpdwFirstScheme: LPDWORD;
  pdwAuthTarget: LPDWORD
): BOOL; stdcall;
{$EXTERNALSYM WinHttpQueryAuthSchemes}
function WinHttpQueryDataAvailable(
  hRequest: HINTERNET;
  lpdwNumberOfBytesAvailable: LPDWORD
): BOOL; stdcall;
{$EXTERNALSYM WinHttpQueryDataAvailable}
function WinHttpQueryHeaders(
  hRequest: HINTERNET;
  dwInfoLevel: DWORD;
  pwszName: LPCWSTR;
  lpBuffer: LPVOID;
  lpdwBufferLength: LPDWORD;
  lpdwIndex: LPDWORD
): BOOL; stdcall;
{$EXTERNALSYM WinHttpQueryHeaders}
function WinHttpQueryOption(
  hInternet: HINTERNET;
  dwOption: DWORD;
  lpBuffer: LPVOID;
  lpdwBufferLength: LPDWORD
): BOOL; stdcall;
{$EXTERNALSYM WinHttpQueryOption}
function WinHttpReadData(
  hRequest: HINTERNET;
  lpBuffer: LPVOID;
  dwNumberOfBytesToRead: DWORD;
  lpdwNumberOfBytesRead: LPDWORD
): BOOL; stdcall;
{$EXTERNALSYM WinHttpReadData}
function WinHttpReceiveResponse(
  hRequest: HINTERNET;
  lpReserved: LPVOID
): BOOL; stdcall;
{$EXTERNALSYM WinHttpReceiveResponse}
function WinHttpSendRequest(
  hRequest: HINTERNET;
  pwszHeaders: LPCWSTR;
  dwHeadersLength: DWORD;
  lpOptional: LPVOID;
  dwOptionalLength: DWORD;
  dwTotalLength: DWORD;
  dwContext: DWORD_PTR
): BOOL; stdcall;
{$EXTERNALSYM WinHttpSendRequest}
function WinHttpSetCredentials(
  hRequest: HINTERNET;
  AuthTargets: DWORD;
  AuthScheme: DWORD;
  pwszUserName: LPCWSTR;
  pwszPassword: LPCWSTR;
  pAuthParams: LPVOID
): BOOL; stdcall;
{$EXTERNALSYM WinHttpSetCredentials}
function WinHttpSetDefaultProxyConfiguration(
  pProxyInfo: PWINHTTP_PROXY_INFO
): BOOL; stdcall;
{$EXTERNALSYM WinHttpSetDefaultProxyConfiguration}
function WinHttpSetOption(
  hInternet: HINTERNET;
  dwOption: DWORD;
  lpBuffer: LPVOID;
  dwBufferLength: DWORD
): BOOL; stdcall;
{$EXTERNALSYM WinHttpSetOption}
function WinHttpSetStatusCallback(
  hInternet: HINTERNET;
  lpfnInternetCallback: WINHTTP_STATUS_CALLBACK;
  dwNotificationFlags: DWORD;
  dwReserved: DWORD_PTR
): WINHTTP_STATUS_CALLBACK; stdcall;
{$EXTERNALSYM WinHttpSetStatusCallback}
function WinHttpSetTimeouts(
  hInternet: HINTERNET;
  dwResolveTimeout: int;
  dwConnectTimeout: int;
  dwSendTimeout: int;
  dwReceiveTimeout: int
): BOOL; stdcall;
{$EXTERNALSYM WinHttpSetTimeouts}
function WinHttpTimeFromSystemTime(
  pst: PSYSTEMTIME;
  pwszTime: LPWSTR
): BOOL; stdcall;
{$EXTERNALSYM WinHttpTimeFromSystemTime}
function WinHttpTimeToSystemTime(
  pwszTime: LPCWSTR;
  pst: PSYSTEMTIME
): BOOL; stdcall;
{$EXTERNALSYM WinHttpTimeToSystemTime}
function WinHttpWriteData(
  hRequest: HINTERNET;
  lpBuffer: LPCVOID;
  dwNumberOfBytesToWrite: DWORD;
  lpdwNumberOfBytesWritten: LPDWORD
): BOOL; stdcall;
{$EXTERNALSYM WinHttpWriteData}


implementation

const
   winhttpdll = 'Winhttp.dll';

   UNABLE_TO_LOAD_DLL = 'Unable to load '+winhttpdll;

{$IFDEF WINHTTP_DLL_STATIC}

function WinHttpAddRequestHeaders;               external winhttpdll name 'WinHttpAddRequestHeaders';
function WinHttpCheckPlatform;                   external winhttpdll name 'WinHttpCheckPlatform';
function WinHttpCloseHandle;                     external winhttpdll name 'WinHttpCloseHandle';
function WinHttpConnect;                         external winhttpdll name 'WinHttpConnect';
function WinHttpCrackUrl;                        external winhttpdll name 'WinHttpCrackUrl';
function WinHttpCreateUrl;                       external winhttpdll name 'WinHttpCreateUrl';
function WinHttpDetectAutoProxyConfigUrl;        external winhttpdll name 'WinHttpDetectAutoProxyConfigUrl';
function WinHttpGetDefaultProxyConfiguration;    external winhttpdll name 'WinHttpGetDefaultProxyConfiguration';
function WinHttpGetIEProxyConfigForCurrentUser;  external winhttpdll name 'WinHttpGetIEProxyConfigForCurrentUser';
function WinHttpGetProxyForUrl;                  external winhttpdll name 'WinHttpGetProxyForUrl';
function WinHttpOpen;                            external winhttpdll name 'WinHttpOpen';
function WinHttpOpenRequest;                     external winhttpdll name 'WinHttpOpenRequest';
function WinHttpQueryAuthSchemes;                external winhttpdll name 'WinHttpQueryAuthSchemes';
function WinHttpQueryDataAvailable;              external winhttpdll name 'WinHttpQueryDataAvailable';
function WinHttpQueryHeaders;                    external winhttpdll name 'WinHttpQueryHeaders';
function WinHttpQueryOption;                     external winhttpdll name 'WinHttpQueryOption';
function WinHttpReadData;                        external winhttpdll name 'WinHttpReadData';
function WinHttpReceiveResponse;                 external winhttpdll name 'WinHttpReceiveResponse';
function WinHttpSendRequest;                     external winhttpdll name 'WinHttpSendRequest';
function WinHttpSetCredentials;                  external winhttpdll name 'WinHttpSetCredentials';
function WinHttpSetDefaultProxyConfiguration;    external winhttpdll name 'WinHttpSetDefaultProxyConfiguration';
function WinHttpSetOption;                       external winhttpdll name 'WinHttpSetOption';
function WinHttpSetStatusCallback;               external winhttpdll name 'WinHttpSetStatusCallback';
function WinHttpSetTimeouts;                     external winhttpdll name 'WinHttpSetTimeouts';
function WinHttpTimeFromSystemTime;              external winhttpdll name 'WinHttpTimeFromSystemTime';
function WinHttpTimeToSystemTime;                external winhttpdll name 'WinHttpTimeToSystemTime';
function WinHttpWriteData;                       external winhttpdll name 'WinHttpWriteData';

{$ELSE}

var
  libWinHttpDLLHandle: THandle;

type

TWinHttpAddRequestHeaders = function (
  hRequest: HINTERNET;
  pwszHeaders: LPCWSTR;
  dwHeadersLength: DWORD;
  dwModifiers: DWORD
): BOOL; stdcall; 

TWinHttpCheckPlatform = function (): BOOL; stdcall;

TWinHttpCloseHandle = function (
  hInternet: HINTERNET
): BOOL; stdcall; 

TWinHttpConnect = function (
  hSession: HINTERNET;
  pswzServerName: LPCWSTR;
  nServerPort: INTERNET_PORT;
  dwReserved: DWORD
): HINTERNET; stdcall; 

TWinHttpCrackUrl = function (
  pwszUrl: LPCWSTR;
  dwUrlLength: DWORD;
  dwFlags: DWORD;
  lpUrlComponents: LPURL_COMPONENTS
): BOOL; stdcall; 

TWinHttpCreateUrl = function (
  lpUrlComponents: LPURL_COMPONENTS;
  dwFlags: DWORD;
  pwszUrl: LPWSTR;
  lpdwUrlLength: LPDWORD
): BOOL; stdcall; 

TWinHttpDetectAutoProxyConfigUrl = function (
  dwAutoDetectFlags: DWORD;
  ppwszAutoConfigUrl: PLPWSTR
): BOOL; stdcall; 

TWinHttpGetDefaultProxyConfiguration = function (
  pProxyInfo: PWINHTTP_PROXY_INFO
): BOOL; stdcall; 

TWinHttpGetIEProxyConfigForCurrentUser = function (
  pProxyConfig: PWINHTTP_CURRENT_USER_IE_PROXY_CONFIG
): BOOL; stdcall; 

TWinHttpGetProxyForUrl = function (
  hSession: HINTERNET;
  lpcwszUrl: LPCWSTR;
  pAutoProxyOptions: PWINHTTP_AUTOPROXY_OPTIONS;
  pProxyInfo: PWINHTTP_PROXY_INFO
): BOOL; stdcall; 

TWinHttpOpen = function (
  pwszUserAgent: LPCWSTR;
  dwAccessType: DWORD;
  pwszProxyName: LPCWSTR;
  pwszProxyBypass: LPCWSTR;
  dwFlags: DWORD
): HINTERNET; stdcall; 

TWinHttpOpenRequest = function (
  hConnect: HINTERNET;
  pwszVerb: LPCWSTR;
  pwszObjectName: LPCWSTR;
  pwszVersion: LPCWSTR;
  pwszReferrer: LPCWSTR;
  ppwszAcceptTypes: PLPCWSTR;
  dwFlags: DWORD
): HINTERNET; stdcall; 

TWinHttpQueryAuthSchemes = function (
  hRequest: HINTERNET;
  lpdwSupportedSchemes: LPDWORD;
  lpdwFirstScheme: LPDWORD;
  pdwAuthTarget: LPDWORD
): BOOL; stdcall; 

TWinHttpQueryDataAvailable = function (
  hRequest: HINTERNET;
  lpdwNumberOfBytesAvailable: LPDWORD
): BOOL; stdcall; 

TWinHttpQueryHeaders = function (
  hRequest: HINTERNET;
  dwInfoLevel: DWORD;
  pwszName: LPCWSTR;
  lpBuffer: LPVOID;
  lpdwBufferLength: LPDWORD;
  lpdwIndex: LPDWORD
): BOOL; stdcall; 

TWinHttpQueryOption = function (
  hInternet: HINTERNET;
  dwOption: DWORD;
  lpBuffer: LPVOID;
  lpdwBufferLength: LPDWORD
): BOOL; stdcall; 

TWinHttpReadData = function (
  hRequest: HINTERNET;
  lpBuffer: LPVOID;
  dwNumberOfBytesToRead: DWORD;
  lpdwNumberOfBytesRead: LPDWORD
): BOOL; stdcall; 

TWinHttpReceiveResponse = function (
  hRequest: HINTERNET;
  lpReserved: LPVOID
): BOOL; stdcall; 

TWinHttpSendRequest = function (
  hRequest: HINTERNET;
  pwszHeaders: LPCWSTR;
  dwHeadersLength: DWORD;
  lpOptional: LPVOID;
  dwOptionalLength: DWORD;
  dwTotalLength: DWORD;
  dwContext: DWORD_PTR
): BOOL; stdcall;

TWinHttpSetCredentials = function (
  hRequest: HINTERNET;
  AuthTargets: DWORD;
  AuthScheme: DWORD;
  pwszUserName: LPCWSTR;
  pwszPassword: LPCWSTR;
  pAuthParams: LPVOID
): BOOL; stdcall;

TWinHttpSetDefaultProxyConfiguration = function (
  pProxyInfo: PWINHTTP_PROXY_INFO
): BOOL; stdcall;

TWinHttpSetOption = function (
  hInternet: HINTERNET;
  dwOption: DWORD;
  lpBuffer: LPVOID;
  dwBufferLength: DWORD
): BOOL; stdcall; 

TWinHttpSetStatusCallback = function (
  hInternet: HINTERNET;
  lpfnInternetCallback: WINHTTP_STATUS_CALLBACK;
  dwNotificationFlags: DWORD;
  dwReserved: DWORD_PTR
): WINHTTP_STATUS_CALLBACK; stdcall; 

TWinHttpSetTimeouts = function (
  hInternet: HINTERNET;
  dwResolveTimeout: int;
  dwConnectTimeout: int;
  dwSendTimeout: int;
  dwReceiveTimeout: int
): BOOL; stdcall; 

TWinHttpTimeFromSystemTime = function (
  pst: PSYSTEMTIME;
  pwszTime: LPWSTR
): BOOL; stdcall; 

TWinHttpTimeToSystemTime = function (
  pwszTime: LPCWSTR;
  pst: PSYSTEMTIME
): BOOL; stdcall; 

TWinHttpWriteData = function (
  hRequest: HINTERNET;
  lpBuffer: LPCVOID;
  dwNumberOfBytesToWrite: DWORD;
  lpdwNumberOfBytesWritten: LPDWORD
): BOOL; stdcall;



function WinHttpAddRequestHeaders(
  hRequest: HINTERNET;
  pwszHeaders: LPCWSTR;
  dwHeadersLength: DWORD;
  dwModifiers: DWORD
): BOOL;
var
  libFunction: TWinHttpAddRequestHeaders;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpAddRequestHeaders');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpAddRequestHeaders'' in winhttpdll');
    end;

  Result := libFunction(
                        hRequest,
                        pwszHeaders,
                        dwHeadersLength,
                        dwModifiers
                        );

end;

function WinHttpCheckPlatform(): BOOL;
var
  libFunction: TWinHttpCheckPlatform;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpCheckPlatform');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpCheckPlatform'' in winhttpdll');
    end;

  Result := libFunction();

end;

function WinHttpCloseHandle(
  hInternet: HINTERNET
): BOOL;
var
  libFunction: TWinHttpCloseHandle;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpCloseHandle');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpCloseHandle'' in winhttpdll');
    end;

  Result := libFunction(
                        hInternet
                       );

end;

function WinHttpConnect(
  hSession: HINTERNET;
  pswzServerName: LPCWSTR;
  nServerPort: INTERNET_PORT;
  dwReserved: DWORD
): HINTERNET;
var
  libFunction: TWinHttpConnect;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpConnect');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpConnect'' in winhttpdll');
    end;

  Result := libFunction(
                        hSession,
                        pswzServerName,
                        nServerPort,
                        dwReserved
                       );

end;

function WinHttpCrackUrl(
  pwszUrl: LPCWSTR;
  dwUrlLength: DWORD;
  dwFlags: DWORD;
  lpUrlComponents: LPURL_COMPONENTS
): BOOL;
var
  libFunction: TWinHttpCrackUrl;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpCrackUrl');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpCrackUrl'' in winhttpdll');
    end;

  Result := libFunction(
                        pwszUrl,
                        dwUrlLength,
                        dwFlags,
                        lpUrlComponents
                       );

end;

function WinHttpCreateUrl(
  lpUrlComponents: LPURL_COMPONENTS;
  dwFlags: DWORD;
  pwszUrl: LPWSTR;
  lpdwUrlLength: LPDWORD
): BOOL;
var
  libFunction: TWinHttpCreateUrl;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpCreateUrl');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpCreateUrl'' in winhttpdll');
    end;

  Result := libFunction(
                        lpUrlComponents,
                        dwFlags,
                        pwszUrl,
                        lpdwUrlLength
                       );

end;

function WinHttpDetectAutoProxyConfigUrl(
  dwAutoDetectFlags: DWORD;
  ppwszAutoConfigUrl: PLPWSTR
): BOOL;
var
  libFunction: TWinHttpDetectAutoProxyConfigUrl;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpDetectAutoProxyConfigUrl');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpDetectAutoProxyConfigUrl'' in winhttpdll');
    end;

  Result := libFunction(
                        dwAutoDetectFlags,
                        ppwszAutoConfigUrl
                       );

end;

function WinHttpGetDefaultProxyConfiguration(
  pProxyInfo: PWINHTTP_PROXY_INFO
): BOOL;
var
  libFunction: TWinHttpGetDefaultProxyConfiguration;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpGetDefaultProxyConfiguration');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpGetDefaultProxyConfiguration'' in winhttpdll');
    end;

  Result := libFunction(
                        pProxyInfo
                       );

end;

function WinHttpGetIEProxyConfigForCurrentUser(
  pProxyConfig: PWINHTTP_CURRENT_USER_IE_PROXY_CONFIG
): BOOL;
var
  libFunction: TWinHttpGetIEProxyConfigForCurrentUser;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpGetIEProxyConfigForCurrentUser');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpGetIEProxyConfigForCurrentUser'' in winhttpdll');
    end;

  Result := libFunction(
                        pProxyConfig
                       );

end;

function WinHttpGetProxyForUrl(
  hSession: HINTERNET;
  lpcwszUrl: LPCWSTR;
  pAutoProxyOptions: PWINHTTP_AUTOPROXY_OPTIONS;
  pProxyInfo: PWINHTTP_PROXY_INFO
): BOOL;
var
  libFunction: TWinHttpGetProxyForUrl;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpGetProxyForUrl');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpGetProxyForUrl'' in winhttpdll');
    end;

  Result := libFunction(
                        hSession,
                        lpcwszUrl,
                        pAutoProxyOptions,
                        pProxyInfo
                       );

end;

function WinHttpOpen(
  pwszUserAgent: LPCWSTR;
  dwAccessType: DWORD;
  pwszProxyName: LPCWSTR;
  pwszProxyBypass: LPCWSTR;
  dwFlags: DWORD
): HINTERNET;
var
  libFunction: TWinHttpOpen;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpOpen');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpOpen'' in winhttpdll');
    end;

  Result := libFunction(
                        pwszUserAgent,
                        dwAccessType,
                        pwszProxyName,
                        pwszProxyBypass,
                        dwFlags
                       );

end;

function WinHttpOpenRequest(
  hConnect: HINTERNET;
  pwszVerb: LPCWSTR;
  pwszObjectName: LPCWSTR;
  pwszVersion: LPCWSTR;
  pwszReferrer: LPCWSTR;
  ppwszAcceptTypes: PLPCWSTR;
  dwFlags: DWORD
): HINTERNET;
var
  libFunction: TWinHttpOpenRequest;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpOpenRequest');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpOpenRequest'' in winhttpdll');
    end;

  Result := libFunction(
                        hConnect,
                        pwszVerb,
                        pwszObjectName,
                        pwszVersion,
                        pwszReferrer,
                        ppwszAcceptTypes,
                        dwFlags
                       );

end;

function WinHttpQueryAuthSchemes(
  hRequest: HINTERNET;
  lpdwSupportedSchemes: LPDWORD;
  lpdwFirstScheme: LPDWORD;
  pdwAuthTarget: LPDWORD
): BOOL;
var
  libFunction: TWinHttpQueryAuthSchemes;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpQueryAuthSchemes');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpQueryAuthSchemes'' in winhttpdll');
    end;

  Result := libFunction(
                        hRequest,
                        lpdwSupportedSchemes,
                        lpdwFirstScheme,
                        pdwAuthTarget
                       );

end;

function WinHttpQueryDataAvailable(
  hRequest: HINTERNET;
  lpdwNumberOfBytesAvailable: LPDWORD
): BOOL;
var
  libFunction: TWinHttpQueryDataAvailable;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpQueryDataAvailable');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpQueryDataAvailable'' in winhttpdll');
    end;

  Result := libFunction(
                        hRequest,
                        lpdwNumberOfBytesAvailable
                       );

end;

function WinHttpQueryHeaders(
  hRequest: HINTERNET;
  dwInfoLevel: DWORD;
  pwszName: LPCWSTR;
  lpBuffer: LPVOID;
  lpdwBufferLength: LPDWORD;
  lpdwIndex: LPDWORD
): BOOL;
var
  libFunction: TWinHttpQueryHeaders;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpQueryHeaders');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpQueryHeaders'' in winhttpdll');
    end;

  Result := libFunction(
                        hRequest,
                        dwInfoLevel,
                        pwszName,
                        lpBuffer,
                        lpdwBufferLength,
                        lpdwIndex
                       );

end;

function WinHttpQueryOption(
  hInternet: HINTERNET;
  dwOption: DWORD;
  lpBuffer: LPVOID;
  lpdwBufferLength: LPDWORD
): BOOL;
var
  libFunction: TWinHttpQueryOption;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpQueryOption');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpQueryOption'' in winhttpdll');
    end;

  Result := libFunction(
                        hInternet,
                        dwOption,
                        lpBuffer,
                        lpdwBufferLength
                       );

end;

function WinHttpReadData(
  hRequest: HINTERNET;
  lpBuffer: LPVOID;
  dwNumberOfBytesToRead: DWORD;
  lpdwNumberOfBytesRead: LPDWORD
): BOOL;
var
  libFunction: TWinHttpReadData;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpReadData');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpReadData'' in winhttpdll');
    end;

  Result := libFunction(
                        hRequest,
                        lpBuffer,
                        dwNumberOfBytesToRead,
                        lpdwNumberOfBytesRead
                       );

end;

function WinHttpReceiveResponse(
  hRequest: HINTERNET;
  lpReserved: LPVOID
): BOOL;
var
  libFunction: TWinHttpReceiveResponse;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpReceiveResponse');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpReceiveResponse'' in winhttpdll');
    end;

  Result := libFunction(
                        hRequest,
                        lpReserved
                       );

end;

function WinHttpSendRequest(
  hRequest: HINTERNET;
  pwszHeaders: LPCWSTR;
  dwHeadersLength: DWORD;
  lpOptional: LPVOID;
  dwOptionalLength: DWORD;
  dwTotalLength: DWORD;
  dwContext: DWORD_PTR
): BOOL;
var
  libFunction: TWinHttpSendRequest;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpSendRequest');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpSendRequest'' in winhttpdll');
    end;

  Result := libFunction(
                        hRequest,
                        pwszHeaders,
                        dwHeadersLength,
                        lpOptional,
                        dwOptionalLength,
                        dwTotalLength,
                        dwContext
                       );

end;

function WinHttpSetCredentials(
  hRequest: HINTERNET;
  AuthTargets: DWORD;
  AuthScheme: DWORD;
  pwszUserName: LPCWSTR;
  pwszPassword: LPCWSTR;
  pAuthParams: LPVOID
): BOOL;
var
  libFunction: TWinHttpSetCredentials;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpSetCredentials');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpSetCredentials'' in winhttpdll');
    end;

  Result := libFunction(
                        hRequest,
                        AuthTargets,
                        AuthScheme,
                        pwszUserName,
                        pwszPassword,
                        pAuthParams
                       );

end;

function WinHttpSetDefaultProxyConfiguration(
  pProxyInfo: PWINHTTP_PROXY_INFO
): BOOL;
var
  libFunction: TWinHttpSetDefaultProxyConfiguration;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpSetDefaultProxyConfiguration');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpSetDefaultProxyConfiguration'' in winhttpdll');
    end;

  Result := libFunction(
                        pProxyInfo
                       );

end;

function WinHttpSetOption(
  hInternet: HINTERNET;
  dwOption: DWORD;
  lpBuffer: LPVOID;
  dwBufferLength: DWORD
): BOOL;
var
  libFunction: TWinHttpSetOption;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpSetOption');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpSetOption'' in winhttpdll');
    end;

  Result := libFunction(
                        hInternet,
                        dwOption,
                        lpBuffer,
                        dwBufferLength
                       );

end;

function WinHttpSetStatusCallback(
  hInternet: HINTERNET;
  lpfnInternetCallback: WINHTTP_STATUS_CALLBACK;
  dwNotificationFlags: DWORD;
  dwReserved: DWORD_PTR
): WINHTTP_STATUS_CALLBACK;
var
  libFunction: TWinHttpSetStatusCallback;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpSetStatusCallback');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpSetStatusCallback'' in winhttpdll');
    end;

  Result := libFunction(
                        hInternet,
                        lpfnInternetCallback,
                        dwNotificationFlags,
                        dwReserved
                       );

end;

function WinHttpSetTimeouts(
  hInternet: HINTERNET;
  dwResolveTimeout: int;
  dwConnectTimeout: int;
  dwSendTimeout: int;
  dwReceiveTimeout: int
): BOOL;
var
  libFunction: TWinHttpSetTimeouts;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpSetTimeouts');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpSetTimeouts'' in winhttpdll');
    end;

  Result := libFunction(
                        hInternet,
                        dwResolveTimeout,
                        dwConnectTimeout,
                        dwSendTimeout,
                        dwReceiveTimeout
                       );

end;

function WinHttpTimeFromSystemTime(
  pst: PSYSTEMTIME;
  pwszTime: LPWSTR
): BOOL;
var
  libFunction: TWinHttpTimeFromSystemTime;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpTimeFromSystemTime');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpTimeFromSystemTime'' in winhttpdll');
    end;

  Result := libFunction(
                        pst,
                        pwszTime
                       );

end;

function WinHttpTimeToSystemTime(
  pwszTime: LPCWSTR;
  pst: PSYSTEMTIME
): BOOL;
var
  libFunction: TWinHttpTimeToSystemTime;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpTimeToSystemTime');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpTimeToSystemTime'' in winhttpdll');
    end;

  Result := libFunction(
                        pwszTime,
                        pst
                       );

end;

function WinHttpWriteData(
  hRequest: HINTERNET;
  lpBuffer: LPCVOID;
  dwNumberOfBytesToWrite: DWORD;
  lpdwNumberOfBytesWritten: LPDWORD
): BOOL;
var
  libFunction: TWinHttpWriteData;
begin
  if libWinHttpDLLHandle = 0 then
    begin
    raise EWinhttpException.Create(UNABLE_TO_LOAD_DLL);
    end;

  @libFunction := GetProcAddress(libWinHttpDLLHandle, 'WinHttpWriteData');
  if @libFunction = nil then
    begin
    raise EWinhttpException.Create('Unable to locate ''WinHttpWriteData'' in winhttpdll');
    end;

  Result := libFunction(
                        hRequest,
                        lpBuffer,
                        dwNumberOfBytesToWrite,
                        lpdwNumberOfBytesWritten
                       );

end;

{$ENDIF}

initialization
  libWinHttpDLLHandle  := LoadLibrary(winhttpdll);

finalization
  if (libWinHttpDLLHandle <> 0) then
    begin
    FreeLibrary(libWinHttpDLLHandle);
    end;

END.

