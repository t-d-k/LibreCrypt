// ------------------------
// "Private"
function GetRandom(len: integer): string;

function RepeatedHash(
                      hashKernelModeDeviceName: Ansistring;
                      hashGUID: TGUID;
                      input: Ansistring;
                      var output: Ansistring
                     ): boolean;
function Process(
                 stripeData: Ansistring;
                 totalBlockCount: integer;
                 blockLength: integer;
                 hashKernelModeDeviceName: string;
                 hashGUID: TGUID;
                 var output: Ansistring
                ): boolean;

function GetBlock(stripeData: Ansistring; totalBlockCount: integer; blockLength: integer; blockNumber: integer; var output: Ansistring): boolean;


// ------------------------
// "Public"

function AFSplit(
                 input: AnsiString;
                 n: integer;
                 hashKernelModeDeviceName: string;
                 hashGUID: TGUID;
                 var output: AnsiString
                ): boolean;

function AFMerge(
                 input: AnsiString;
                 n: integer;
                 hashKernelModeDeviceName: string;
                 hashGUID: TGUID;
                 var output: AnsiString
                ): boolean;


// ------------------------


