// A LPNMSORTLISTVIEW is sent with the SLVN_SORTCHANGED notification
typedef struct tagNMSORTLISTVIEW
{
    NMHDR hdr;
    int iNewSortColumn;
    int iOldSortColumn;
} NMSORTLISTVIEW, *LPNMSORTLISTVIEW;

