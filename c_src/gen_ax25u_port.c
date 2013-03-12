#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ei.h>
#include <erl_interface.h>
#define PACKET_N 2
#define BINARY_VERSION 131
#define BUF_SIZE 1024

#define ERR_READ_CMD       101
#define ERR_UNKNOWN_CMD    102
#define ERR_MESSAGE_HDR    103
#define ERR_REC_NOT_TUPLE  111
#define ERR_REC_NO_TUPHDR  112
#define ERR_REC_NOT_ATOM   113
#define ERR_REC_ATOM       114

int read_command(char* buf);
int decode_record(char *buf, int len, int *ptr, char* name, int* arity);
int decode_message_hdr(char* buf, int len, int* ptr);

int main(int argn, char** argv)
{
    int rc;
    char buf[BUF_SIZE];
    int len;
    int ptr;
    char recName[BUF_SIZE];
    int recArity = 0;
    
    erl_init(0, 0);

    while (1)
    {
        if ((len = read_command(buf)) <= 0)
            return ERR_READ_CMD;

        ptr = 0;
        if ((rc = decode_message_hdr(buf, len, &ptr)) != 0)
            return rc;

        if ((rc = decode_record(buf, len, &ptr, recName, &recArity)) != 0)
            return rc;

        if (strcmp("stop", recName) == 0 && recArity == 1)
        {
            break;
        }
        else if (strcmp("info", recName) == 0 && recArity == 2)
        {
            /* read PID */
            /* send_info */
        }
        else
        {
            return ERR_UNKNOWN_CMD;
        }
    }
    
    return EXIT_SUCCESS;
}

/**
 *  Tries to decode record. On success, function will return 0,
 *  ptr will point to the second element of the record and
 *  name and arity will be filled with record name and arity.
 */
int decode_record(char *buf, int len, int *ptr, char* name, int* arity)
{
    int eirc;
    int termType = 0;
    int termSize = 0;

    eirc = ei_get_type(buf, ptr, &termType, &termSize);
    if (!(eirc == 0 && (termType == ERL_SMALL_TUPLE_EXT || termType == ERL_LARGE_TUPLE_EXT)))
        return ERR_REC_NOT_TUPLE;

    eirc = ei_decode_tuple_header(buf, ptr, arity);
    if (eirc != 0)
        return ERR_REC_NO_TUPHDR;

    eirc = ei_get_type(buf, ptr, &termType, &termSize);
    if (!(eirc == 0 && termType == ERL_ATOM_EXT))
        return ERR_REC_NOT_ATOM;

    eirc = ei_decode_atom(buf, ptr, name);
    if (eirc != 0)
        return ERR_REC_ATOM;

    return 0;
}

/**
 *  Reads and checks messsage header fields.
 */
int decode_message_hdr(char* buf, int len, int* ptr)
{
    int eirc;
    int binVersion = 0;

    eirc = ei_decode_version(buf, ptr, &binVersion);
    if (eirc == -1 || binVersion != BINARY_VERSION)
        return ERR_MESSAGE_HDR;

    return 0;
}

/*
 *  Generic ErlangInterface IO functions.
 *  Initial code is taken from http://www.erlang.org/doc/tutorial/c_port.html.
 */

int read_exact(char* buf, int len)
{
    int i, got;
    for (got = 0, i = 0; got < len; got += i)
    {
        if ((i = read(0, buf + got, len - got)) <= 0)
            return i;
    }
    return len;
}

int read_command(char* buf)
{
    int len = 0, i;
    if (read_exact(buf, PACKET_N) != PACKET_N)
        return -1;

    for (len = 0, i = 0; i < PACKET_N; i++)
    {
        len = len << 8 | buf[i];
    }
    return read_exact(buf, len);
}

