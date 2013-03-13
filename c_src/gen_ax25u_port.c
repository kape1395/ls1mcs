#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ei.h>
#include <erl_interface.h>

#define PACKET_N           2
#define BINARY_VERSION     131
#define BUF_SIZE           1024

#define ERR_READ_CMD       101
#define ERR_UNKNOWN_CMD    102
#define ERR_MESSAGE_HDR    103
#define ERR_REC_NOT_TUPLE  111
#define ERR_REC_NO_TUPHDR  112
#define ERR_REC_NOT_ATOM   113
#define ERR_REC_ATOM       114
#define ERR_CMD_INFO_PID   200
#define ERR_CMD_SEND_MSG   201

int handle_cmd_info(char *buf, int len, int *ptr);
int handle_cmd_send(char *buf, int len, int *ptr);

int decode_record(char *buf, int len, int *ptr, char* name, int* arity);
int decode_message_hdr(char* buf, int len, int* ptr);

int read_command(char* buf);
int write_command(char *buf, int len);

/**
 *  Entry point.
 */
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
            if ((rc = handle_cmd_stop(buf, len, &ptr)) != 0)
                return rc;
            return EXIT_SUCCESS;
        }
        else if (strcmp("info", recName) == 0 && recArity == 2)
        {
            if ((rc = handle_cmd_info(buf, len, &ptr)) != 0)
                return rc;
        }
        else if (strcmp("send", recName) == 0 && recArity == 2)
        {
            if ((rc = handle_cmd_send(buf, len, &ptr)) != 0)
                return rc;
        }
        else
        {
            return ERR_UNKNOWN_CMD;
        }
    }
}


/* ************************************************************************** */

/**
 *  Handles STOP command.
 */
int handle_cmd_info(char *buf, int len, int *ptr)
{
    /* TODO: Stop thread, join, close socket */
}

/**
 *  Handles INFO command.
 */
int handle_cmd_info(char *buf, int len, int *ptr)
{
    erlang_pid pid;
    if (ei_decode_pid(buf, ptr, &pid) == -1)
        return ERR_CMD_INFO_PID;

    *ptr = 0;
    ei_encode_version(buf, ptr);
    ei_encode_tuple_header(buf, ptr, 3);
    ei_encode_atom(buf, ptr, "info");
    ei_encode_string(buf, ptr, "call-1");
    ei_encode_string(buf, ptr, "call-2");
    len = *ptr;
    write_command(buf, len);
    return 0;
}

/**
 *  Handles SEND command.
 */
int handle_cmd_send(char *buf, int len, int *ptr)
{
    char message[BUF_SIZE];
    long msg_len = 0;
    if (ei_decode_binary(buf, ptr, message, msg_len) == -1)
        return ERR_CMD_SEND_MSG;

    return 0;
}


/* ************************************************************************** */

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

/* ************************************************************************** */
/*
 *  Generic ErlangInterface IO functions.
 *  Initial code is taken from http://www.erlang.org/doc/tutorial/c_port.html.
 */

int read_exact(char* buf, int len);
int write_exact(char *buf, int len);

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

int write_command(char *buf, int len)
{
    unsigned m = 0xff;
    unsigned l = (unsigned) len;
    int i;
    char c;
    for (i = PACKET_N - 1; i >= 0; i--)
    {
        c = (char) (l >> (8 * i)) & m;
        write_exact(&c, 1);
    }
    return write_exact(buf, len);
}

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

int write_exact(char *buf, int len)
{
    int i, wrote;
    for (i = 0, wrote = 0; wrote < len; wrote += i)
    {
        if ((i = write(1, buf + wrote, len - wrote)) <= 0)
            return i;
    }
    return len;
}

/* ************************************************************************** */
