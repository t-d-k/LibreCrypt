// ------------------------
// "Private"
function GetRandom(len: integer): string;

function RepeatedHash(
                      hashKernelModeDeviceName: string;
                      hashGUID: TGUID;
                      input: string;
                      var output: string
                     ): boolean;
function Process(
                 stripeData: string;
                 totalBlockCount: integer;
                 blockLength: integer;
                 hashKernelModeDeviceName: string;
                 hashGUID: TGUID;
                 var output: string
                ): boolean;

function GetBlock(stripeData: string; totalBlockCount: integer; blockLength: integer; blockNumber: integer; var output: string): boolean;


// ------------------------
// "Public"

function AFSplit(
                 input: string;
                 n: integer;
                 hashKernelModeDeviceName: string;
                 hashGUID: TGUID;
                 var output: string
                ): boolean;

function AFMerge(
                 input: string;
                 n: integer;
                 hashKernelModeDeviceName: string;
                 hashGUID: TGUID;
                 var output: string
                ): boolean;


// ------------------------


