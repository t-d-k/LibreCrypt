unit PKCS11KnownLibs;

interface

uses
  Classes;

type
  TPKCS11KnownLibrary = record
    DLLFilename: string;

    Manufacturer: string;
    Device: string;
  end;

const
  PKCS11_KNOWN_LIBRARIES: array [1..68] of TPKCS11KnownLibrary = (
                           //this is by far the most common as included in forefox, so comes first
                             //todo: can include in app? mpl?
                           (
                           DLLFilename: 'softokn3.dll';
                           Manufacturer: 'Mozilla/Netscape';
                           Device: 'Mozilla or Netscape crypto module';
                          ),
                          (
                           DLLFilename: 'acospkcs11.dll';
                           Manufacturer: 'ACS';
                           Device: 'ACOS5 smartcards';
                          ),
                          (
                           DLLFilename: 'aetpkss1.dll';
                           Manufacturer: 'AET';
                           Device: 'Rainbow iKey 3000 series and G&D StarCos 2.3 SPK cards';
                          ),
                          (
                           DLLFilename: 'etpkcs11.dll';
                           Manufacturer: 'Aladdin';
                           Device: 'eToken PRO';
                          ),
                          (
                           DLLFilename: 'etpkcs11.dll';
                           Manufacturer: 'Aladdin';
                           Device: 'eToken R2';
                          ),
                          (
                           DLLFilename: 'sadaptor.dll';
                           Manufacturer: 'Algorithmic Research';
                           Device: 'MiniKey';
                          ),
                          (
                           DLLFilename: 'aloaha_pkcs11.dll';
                           Manufacturer: 'Aloaha';
                           Device: 'Smart Card Connector';
                          ),
                          (
                           DLLFilename: 'psepkcs11.dll';
                           Manufacturer: 'A-Sign';
                           Device: 'A-Sign premium cards';
                          ),
                          (
                           DLLFilename: 'asepkcs.dll';
                           Manufacturer: 'Athena';
                           Device: 'Athena Smartcard System ASE Card';
                          ),
                          (
                           DLLFilename: 'asignp11.dll';
                           Manufacturer: 'A-Trust';
                           Device: 'a-sign';
                          ),
                          (
                           DLLFilename: 'Belgium Identity Card PKCS11.dll';
                           Manufacturer: 'Belgian Government';
                           Device: 'Belgian Electronic Identity (eID) Card';
                          ),
                          (
                           DLLFilename: 'cryst32.dll';
                           Manufacturer: 'Chrysalis';
                           Device: '';
                          ),
                          (
                           DLLFilename: 'cryst201.dll';
                           Manufacturer: 'Chrysalis';
                           Device: 'LUNA';
                          ),
                          (
                           DLLFilename: 'dspkcs.dll';
                           Manufacturer: 'Dallas Semiconductors';
                           Device: 'iButton';
                          ),
                          (
                           DLLFilename: 'cryptoki.dll';
                           Manufacturer: 'Eracom';
                           Device: '(hardware)';
                          ),
                          (
                           DLLFilename: 'cryptoki.dll';
                           Manufacturer: 'Eracom';
                           Device: '(software emulation)';
                          ),
                          (
                           DLLFilename: 'opensc-pkcs11.dll';
                           Manufacturer: 'Estonian Government';
                           Device: 'Estonian Electronic Identity (eID) Card';
                          ),
                          (
                           DLLFilename: 'sadaptor.dll';
                           Manufacturer: 'Eutron';
                           Device: 'Crypto Identity';
                          ),
                          (
                           DLLFilename: 'EP1PK111.DLL';
                           Manufacturer: 'Feitain technologys Co.,Ltd';
                           Device: 'ePass 1000';
                          ),
                          (
                           DLLFilename: 'ep2pk11.dll';
                           Manufacturer: 'Feitain technologys Co.,Ltd';
                           Device: 'ePass 2000';
                          ),
                          (
                           DLLFilename: 'ngp11v211.dll';
                           Manufacturer: 'Feitain technologys Co.,Ltd';
                           Device: 'ePass 2000_FT11';
                          ),
                          (
                           DLLFilename: 'ngp11v211.dll';
                           Manufacturer: 'Feitain technologys Co.,Ltd';
                           Device: 'ePass 3000';
                          ),
                          (
                           DLLFilename: 'ShuttleCsp11_3003.dll';
                           Manufacturer: 'Feitain technologys Co.,Ltd';
                           Device: 'ePass 3003';
                          ),
                          (
                           DLLFilename: 'gclib.dll';
                           Manufacturer: 'Gemplus';
                           Device: '';
                          ),
                          (
                           DLLFilename: 'pk2priv.dll';
                           Manufacturer: 'Gemplus';
                           Device: '';
                          ),
                          (
                           DLLFilename: 'w32pk2ig.dll';
                           Manufacturer: 'GemPlus/GemSoft';
                           Device: 'GemPlus/GemSoft Smartcard';
                          ),
                          (
                           DLLFilename: 'gclib.dll';
                           Manufacturer: 'GemSafe';
                           Device: '';
                          ),
                          (
                           DLLFilename: 'pk2priv.dll';
                           Manufacturer: 'GemSafe';
                           Device: '';
                          ),
                          (
                           DLLFilename: 'cryptoki.dll';
                           Manufacturer: 'IBM';
                           Device: 'IBM 4758';
                          ),
                          (
                           DLLFilename: 'CccSigIT.dll';
                           Manufacturer: 'IBM';
                           Device: 'IBM Digital Signature for the Internet (DSI) for MFC cards';
                          ),
                          (
                           DLLFilename: 'csspkcs11.dll';
                           Manufacturer: 'IBM';
                           Device: 'IBM Embededded Security Subsystem';
                          ),
                          (
                           DLLFilename: 'ibmpkcss.dll';
                           Manufacturer: 'IBM';
                           Device: 'IBM Netfinity PSG Chip1';
                          ),
                          (
                           DLLFilename: 'w32pk2ig.dll';
                           Manufacturer: 'IBM';
                           Device: 'IBM SecureWay Smartcard';
                          ),
                          (
                           DLLFilename: 'cryptoki.dll';
                           Manufacturer: 'IBM';
                           Device: '';
                          ),
                          (
                           DLLFilename: 'id2cbox.dll';
                           Manufacturer: 'ID2';
                           Device: '';
                          ),
                          (
                           DLLFilename: 'cknfast.dll';
                           Manufacturer: 'nCipher';
                           Device: 'nFast';
                          ),
                          (
                           DLLFilename: 'cknfast.dll';
                           Manufacturer: 'nCipher';
                           Device: 'nShield';
                          ),
                          (
                           DLLFilename: 'nxpkcs11.dll';
                           Manufacturer: 'Nexus';
                           Device: '';
                          ),
                          (
                           DLLFilename: 'opensc-pkcs11.dll';
                           Manufacturer: 'OpenSC';
                           Device: '(multiple)';
                          ),
                          (
                           DLLFilename: 'micardoPKCS11.dll';
                           Manufacturer: 'Orga Micardo';
                           Device: '';
                          ),
                          (
                           DLLFilename: 'Cryptoki22.dll';
                           Manufacturer: 'Rainbow';
                           Device: 'CryptoSwift Accelerator';
                          ),
                          (
                           DLLFilename: 'iveacryptoki.dll';
                           Manufacturer: 'Rainbow';
                           Device: 'CryptoSwift HSM';
                          ),
                          (
                           DLLFilename: 'cryptoki22.dll';
                           Manufacturer: 'Rainbow';
                           Device: 'Ikey 1000';
                          ),
                          (
                           DLLFilename: 'k1pk112.dll';
                           Manufacturer: 'Rainbow';
                           Device: 'iKey 1000/1032';
                          ),
                          (
                           DLLFilename: 'dkck201.dll';
                           Manufacturer: 'Rainbow';
                           Device: 'iKey 2000 series and for DataKey cards';
                          ),
                          (
                           DLLFilename: 'dkck232.dll';
                           Manufacturer: 'Rainbow';
                           Device: 'iKey 2000/2032';
                          ),
                          (
                           DLLFilename: 'dkck201.dll';
                           Manufacturer: 'Rainbow';
                           Device: 'iKey 2032';
                          ),
                          (
                           DLLFilename: 'cryptoki22.dll';
                           Manufacturer: 'Rainbow';
                           Device: '';
                          ),
                          (
                           DLLFilename: 'p11card.dll';
                           Manufacturer: 'Safelayer';
                           Device: 'HSM';
                          ),
                          (
                           DLLFilename: 'acpkcs.dll';
                           Manufacturer: 'Schlumberger';
                           Device: 'Cryptoflex';
                          ),
                          (
                           DLLFilename: 'slbck.dll';
                           Manufacturer: 'Schlumberger';
                           Device: 'Cryptoflex';
                          ),
                          (
                           DLLFilename: 'slbck.dll';
                           Manufacturer: 'Schlumberger';
                           Device: 'Cyberflex Access';
                          ),
                          (
                           DLLFilename: 'SetTokI.dll';
                           Manufacturer: 'SeTec';
                           Device: 'SeTokI cards';
                          ),
                          (
                           DLLFilename: 'siecap11.dll';
                           Manufacturer: 'Siemens';
                           Device: 'HiPath SIcurity Card';
                          ),
                          (
                           DLLFilename: 'eTpkcs11.dll';
                           Manufacturer: 'Siemens';
                           Device: 'Some Siemens Card OS cards';
                          ),
                          (
                           DLLFilename: 'smartp11.dll';
                           Manufacturer: 'SmartTrust';
                           Device: '';
                          ),
                          (
                           DLLFilename: 'SpyPK11.dll';
                           Manufacturer: 'Spyrus';
                           Device: '';
                          ),
                          (
                           DLLFilename: 'pkcs201n.dll';
                           Manufacturer: 'Utimaco';
                           Device: 'Cryptoki for SafeGuard';
                          ),
                          (
                           DLLFilename: 'acpkcs.dll';
                           Manufacturer: '';
                           Device: 'ActivCard cards';
                          ),
                          (
                           DLLFilename: 'acpkcs211.dll';
                           Manufacturer: '';
                           Device: 'ActivClient';
                          ),
                          (
                           DLLFilename: 'dkck201.dll';
                           Manufacturer: '';
                           Device: 'Datakey';
                          ),
                          (
                           DLLFilename: 'pkcs201n.dll';
                           Manufacturer: '';
                           Device: 'Datakey';
                          ),
                          (
                           DLLFilename: 'dkck201.dll';
                           Manufacturer: '';
                           Device: 'Datakey CIP';
                          ),
                          (
                           DLLFilename: 'dkck232.dll';
                           Manufacturer: '';
                           Device: 'Datakey/iKey';
                          ),
                          (
                           DLLFilename: 'fort32.dll';
                           Manufacturer: '';
                           Device: 'Fortezza Module';
                          ),
                          (
                           DLLFilename: 'AuCryptoki2-0.dll';
                           Manufacturer: '';
                           Device: 'Oberthur AuthentIC';
                          ),
                          (
                           DLLFilename: '3gp11csp.dll';
                           Manufacturer: '';
                           Device: 'SCW PKCS 3GI 3-G International';
                          ),
                          (
                           DLLFilename: 'pkcs11.dll';
                           Manufacturer: '';
                           Device: 'TeleSec';
                          )
                         );

function PKCS11KnownLibraryPrettyDesc(knownLib: TPKCS11KnownLibrary): string;

implementation

function PKCS11KnownLibraryPrettyDesc(knownLib: TPKCS11KnownLibrary): string;
begin
  Result := knownLib.Manufacturer;
  if (Result = '') then
    begin
    Result := knownLib.Device;
    end
  else if (knownLib.Device <> '') then
    begin
    Result := Result + ': ' + knownLib.Device;
    end;


end;

END.

