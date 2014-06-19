
// Copyright in this code is held by Dr B. R. Gladman but free direct or
// derivative use is permitted subject to acknowledgement of its origin.
// Dr B. R. Gladman                               .   25th January 2000.

#ifdef  __cplusplus

#   define  RIJNDAEL(x) rijndael::##x

    class rijndael : public AES
    {
    private:
        u4byte      k_len;
        u4byte      e_key[64];
        u4byte      d_key[64];
    public:
#else

#   define  RIJNDAEL(x) rijndael_##x

typedef struct RIJNDAEL(_CONTEXT) {
    u4byte  k_len;
    u4byte  e_key[64];
    u4byte  d_key[64];
} RIJNDAEL(CONTEXT);

#endif

    char*   RIJNDAEL(name(void));
    void    RIJNDAEL(set_key)(RIJNDAEL(CONTEXT) *context, const u1byte key[], const u4byte key_len, const enum dir_flag f);
    void    RIJNDAEL(encrypt)(RIJNDAEL(CONTEXT) *context, const u1byte in_blk[], u1byte out_blk[]);
    void    RIJNDAEL(decrypt)(RIJNDAEL(CONTEXT) *context, const u1byte in_blk[], u1byte out_blk[]);

#ifdef  __cplusplus
    };
#endif
