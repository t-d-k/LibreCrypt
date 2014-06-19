
// Copyright in this code is held by Dr B. R. Gladman but free direct or
// derivative use is permitted subject to acknowledgement of its origin.
// Dr B. R. Gladman                               .   25th January 2000.

#ifdef  __cplusplus

#   define  RC6(x)  rc6::##x

    class rc6 : public AES
    {
    private:
        u4byte  l_key[44];
    public:

#else

#   define  RC6(x)  rc6_##x

typedef struct RC6(_CONTEXT) {
    u4byte l_key[44];
} RC6(CONTEXT);

#endif

    char*   RC6(name(void));
    void    RC6(set_key)(RC6(CONTEXT) *context, const u1byte key[], const u4byte key_len, const enum dir_flag f);
    void    RC6(encrypt)(RC6(CONTEXT) *context, const u1byte in_blk[], u1byte out_blk[]);
    void    RC6(decrypt)(RC6(CONTEXT) *context, const u1byte in_blk[], u1byte out_blk[]);

#ifdef  __cplusplus
    };
#endif
