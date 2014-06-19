
// Copyright in this code is held by Dr B. R. Gladman but free direct or
// derivative use is permitted subject to acknowledgement of its origin.
// Dr B. R. Gladman                               .   25th January 2000.

#ifdef  __cplusplus

#   define  SERPENT(x)  serpent::##x

    class serpent : public AES
    {
    private:
        u4byte  l_key[140];
    public:

#else

#   define  SERPENT(x)  serpent_##x

 typedef struct SERPENT(_CONTEXT) {
    u4byte l_key[140];
} SERPENT(CONTEXT);

#endif

    char* SERPENT(name(void));
    void  SERPENT(set_key)(SERPENT(CONTEXT) *context, const u1byte key[], const u4byte key_len, const enum dir_flag f);
    void  SERPENT(encrypt)(SERPENT(CONTEXT) *context, const u1byte in_blk[], u1byte out_blk[]);
    void  SERPENT(decrypt)(SERPENT(CONTEXT) *context, const u1byte in_blk[], u1byte out_blk[]);

#ifdef  __cplusplus
    };
#endif
