

typedef int int32;

typedef MyFancyType MyNonFancyType;

typedef const std::list<int> *IntListPtr;

typedef struct {
	int member;
} MyStruct;

typedef struct S2 {
	int member;
} MyStruct2;

struct S3 {
	int member;
};

typedef struct S3 MyStruct3;

typedef long (FAR PASCAL *_dec_get_impf)( char *lpKvNummer);

typedef long (FAR PASCAL *_ZMSGetKeyTermine)(const char *lpstrKey,const char *lpstrDate, bool bExact,long nKlasse, char *lpstrResult, long nMaxSize);

