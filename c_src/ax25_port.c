#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ei.h>
#include <erl_interface.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netax25/axlib.h>
#include <netax25/ax25.h>
#include <netax25/axconfig.h>


#define PACKET_N            2
#define BINARY_VERSION      131
#define BUF_SIZE            1024
#define SOCK_MODE_SEND      1
#define SOCK_MODE_RECV      2

#define ERR_OK              0
#define ERR_BAD_ARGS        100
#define ERR_READ_CMD        101
#define ERR_UNKNOWN_CMD     102
#define ERR_MESSAGE_HDR     103
#define ERR_MODE_UNKNOWN    104
#define ERR_REC_NOT_TUPLE   111
#define ERR_REC_NO_TUPHDR   112
#define ERR_REC_NOT_ATOM    113
#define ERR_REC_ATOM        114
#define ERR_CMD_INFO_PID    120
#define ERR_CMD_SEND_MSG    121
#define ERR_CMD_UNSUPPORTED 122
#define ERR_RECV_THREAD     130
#define ERR_AX_LOAD         140
#define ERR_AX_ADDR_LOCAL   141
#define ERR_AX_ATON_LOCAL   142
#define ERR_AX_ATON_REMOTE  143

struct state
{
    int             sock_mode;

    pthread_t       thread;
    pthread_mutex_t stop_mutex;
    int             stop_indicator;

    int sockfd;
    char* local_port;
    char* local_call;
    char* remote_call;
    struct full_sockaddr_ax25 local_addr;
    struct full_sockaddr_ax25 remote_addr;
    unsigned local_addr_len;
    unsigned remote_addr_len;
};

void checkerr(int err);
int parse_args(int argn, char **argv, char **local_port, char **remote_call, int *sock_mode);

int handle_cmd_start(char *buf, int len, int *ptr, struct state *state);
int handle_cmd_stop(char *buf, int len, int *ptr, struct state *state);
int handle_cmd_info(char *buf, int len, int *ptr, struct state *state);
int handle_cmd_send(char *buf, int len, int *ptr, struct state *state);
void* handle_sock_recv(void* ptr);

int recv_thread_init(struct state* state);
int recv_thread_join(struct state* state);
int recv_thread_stopping(struct state* state);

int sock_config(struct state* state, char *local_port, char *remote_call);
int sock_open(struct state* state);
int sock_close(struct state* state);
int sock_send(struct state* state, char *buf, int len);
int sock_recv(struct state* state, char *buf, int len, int *msg_len, struct sockaddr * src_addr, socklen_t *src_addr_len);

int decode_record_hdr(char *buf, int len, int *ptr, char* name, int* arity);
int decode_message_hdr(char* buf, int len, int* ptr);

int read_command(char* buf);
int write_command(char *buf, int len);

/**
 *  Entry point.
 */
int main(int argn, char** argv)
{
    char buf[BUF_SIZE];
    int len;
    int ptr;
    char recName[BUF_SIZE];
    int recArity = 0;
    struct state state;

    checkerr(parse_args(argn, argv, &state.local_port, &state.remote_call, &state.sock_mode));

    erl_init(0, 0);

    /**
     *  Assuming start is implicit.
     */
    len = 0;
    ptr = 0;
    checkerr(handle_cmd_start(buf, len, &ptr, &state));

    while (1)
    {
        if ((len = read_command(buf)) <= 0)
            return ERR_READ_CMD;

        ptr = 0;
        checkerr(decode_message_hdr(buf, len, &ptr));
        checkerr(decode_record_hdr(buf, len, &ptr, recName, &recArity));

        if (strcmp("stop", recName) == 0 && recArity == 1)
        {
            checkerr(handle_cmd_stop(buf, len, &ptr, &state));
            return EXIT_SUCCESS;
        }
        else if (strcmp("info", recName) == 0 && recArity == 2)
        {
            checkerr(handle_cmd_info(buf, len, &ptr, &state));
        }
        else if (strcmp("send", recName) == 0 && recArity == 2)
        {
            checkerr(handle_cmd_send(buf, len, &ptr, &state));
        }
        else
        {
            return ERR_UNKNOWN_CMD;
        }
    }
}

/* ************************************************************************** */
/**
 *  Error handling.
 */
void checkerr(int err)
{
    exit(err);
}


/* ************************************************************************** */
/**
 *  Handles command line arguments.
 */
int parse_args(int argn, char **argv, char **local_port, char **remote_call, int *sock_mode)
{
    char *sm;

    if (argn != 4)
        return ERR_BAD_ARGS;

    sm = argv[1];
    *local_port = argv[2];
    *remote_call = argv[3];

    if (strcmp(sm, "send") == 0)
    {
        *sock_mode = SOCK_MODE_SEND;
    }
    else if (strcmp(sm, "recv") == 0)
    {
        *sock_mode = SOCK_MODE_RECV;
    }
    else
    {
        return ERR_MODE_UNKNOWN;
    }
    return ERR_OK;
}


/* ************************************************************************** */

/**
 *  Handles START command.
 */
int handle_cmd_start(char *buf, int len, int *ptr, struct state *state)
{
    checkerr(sock_config(state, state->local_port, state->remote_call));
    checkerr(sock_open(state));

    if (state->sock_mode & SOCK_MODE_RECV)
    {
        checkerr(recv_thread_init(state));
    }

    return ERR_OK;
}

/**
 *  Handles STOP command.
 */
int handle_cmd_stop(char *buf, int len, int *ptr, struct state *state)
{
    fprintf(stderr, "handle_cmd_stop... \n");

    if (state->sock_mode & SOCK_MODE_RECV)
    {
        recv_thread_join(state);
        fprintf(stderr, "handle_cmd_stop... Joined\n");
    }

    sock_close(state);
    fprintf(stderr, "handle_cmd_stop... Socket closed, done\n");

    return ERR_OK;
}

/**
 *  Handles INFO command.
 */
int handle_cmd_info(char *buf, int len, int *ptr, struct state *state)
{
    erlang_pid pid;
    if (ei_decode_pid(buf, ptr, &pid) == -1)
        return ERR_CMD_INFO_PID;

    fprintf(stderr, "handle_cmd_info\n");

    *ptr = 0;
    ei_encode_version(buf, ptr);
    ei_encode_tuple_header(buf, ptr, 3);
    ei_encode_atom(buf, ptr, "info");
    ei_encode_string(buf, ptr, state->local_call);
    ei_encode_string(buf, ptr, state->remote_call);
    len = *ptr;
    write_command(buf, len);
    return ERR_OK;
}

/**
 *  Handles SEND command.
 */
int handle_cmd_send(char *buf, int len, int *ptr, struct state *state)
{
    char message[BUF_SIZE];
    long msg_len = 0;
    int rc;

    if ((state->sock_mode & SOCK_MODE_SEND) == 0)
        return ERR_CMD_UNSUPPORTED;

    if (ei_decode_binary(buf, ptr, message, &msg_len) == -1)
        return ERR_CMD_SEND_MSG;

    fprintf(stderr, "handle_cmd_send: len=%ld\n", msg_len);

    if ((rc = sock_send(state, message, msg_len)) != 0)
        return rc;

    return ERR_OK;
}

/* ************************************************************************** */

/**
 *  Handle Socket RECV.
 */
void* handle_sock_recv(void* ptr)
{
    struct state *state = (struct state*) ptr;
    char recv_buf[BUF_SIZE];
    int  recv_len = 0;
    char term_buf[BUF_SIZE];
    int  term_ptr;
    int  term_len;
    int rc;
    while (1)
    {
        rc = sock_recv(state, recv_buf, BUF_SIZE, &recv_len, NULL, NULL);
        if (rc == 0)
        {
            term_ptr = 0;
            ei_encode_version(term_buf, &term_ptr);
            ei_encode_tuple_header(term_buf, &term_ptr, 2);
            ei_encode_atom(term_buf, &term_ptr, "recv");
            ei_encode_binary(term_buf, &term_ptr, recv_buf, recv_len);
            term_len = term_ptr;
            write_command(term_buf, term_len);
        }
        else if (rc == EAGAIN || rc == EWOULDBLOCK)
        {
            /* OK, just check if we are not stopping yet. */
        }
        else
        {
            fprintf(stderr, "Error wile doing recv, rc=%d\n", rc);
            return NULL;
        }

        if (recv_thread_stopping(state))
            break;
    }
    return NULL;
}


int recv_thread_init(struct state* state)
{
    pthread_mutex_init(&state->stop_mutex, NULL);
    state->stop_indicator = 0;

    if (pthread_create(&state->thread, NULL, handle_sock_recv, state) != 0)
        return ERR_RECV_THREAD;

    return ERR_OK;
}

int recv_thread_join(struct state* state)
{
    pthread_mutex_lock(&state->stop_mutex);
    state->stop_indicator = 1;
    pthread_mutex_unlock(&state->stop_mutex);
    pthread_join(state->thread, NULL);
    return ERR_OK;
}

int recv_thread_stopping(struct state* state)
{
    int stopping;
    pthread_mutex_lock(&state->stop_mutex);
    stopping = state->stop_indicator;
    pthread_mutex_unlock(&state->stop_mutex);
    return stopping;
}


/* ************************************************************************** */

/*
 *  AX.25 Socket functions.
 *  http://www.spinics.net/lists/linux-hams/msg02958.html
 *  apt-get install libax25-dev 
 */

int sock_config(struct state* state, char *local_port, char *remote_call)
{
    memset(&state->local_addr, 0, sizeof(struct full_sockaddr_ax25));
    memset(&state->remote_addr, 0, sizeof(struct full_sockaddr_ax25));

    if (ax25_config_load_ports() == 0)
        return ERR_AX_LOAD;

    if ((state->local_call = ax25_config_get_addr(local_port)) == NULL)
        return ERR_AX_ADDR_LOCAL;

    state->remote_call = remote_call;

    if ((state->local_addr_len = ax25_aton(state->local_call, &state->local_addr)) == -1)
        return ERR_AX_ATON_LOCAL;

    if ((state->remote_addr_len = ax25_aton(state->remote_call, &state->remote_addr)) == -1)
        return ERR_AX_ATON_REMOTE;

    return ERR_OK;
}

int sock_open(struct state* state)
{
    int protocol = 0;
    struct timeval timeout;

    if ((state->sockfd = socket(AF_AX25, SOCK_DGRAM, protocol)) == -1)
        return errno;

    if (bind(state->sockfd, (struct sockaddr *) &state->local_addr, state->local_addr_len) == -1)
        return errno;

    timeout.tv_sec = 1;
    timeout.tv_usec = 0;
    if (setsockopt(state->sockfd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout)) != 0)
        return errno;

    return ERR_OK;
}

int sock_close(struct state* state)
{
    /* TODO: Close. */
    return ERR_OK;
}

int sock_send(struct state* state, char *buf, int len)
{
    fprintf(stderr, "sock_send: len=%d ...\n", len);

    if (sendto(state->sockfd, buf, len, 0, (struct sockaddr *) &state->remote_addr, state->remote_addr_len) == -1)
    {
        fprintf(stderr, "sock_send: len=%d ... errno=%d\n", len, errno);
        return errno;
    }

    fprintf(stderr, "sock_send: len=%d ... Done\n", len);
    return ERR_OK;
}

int sock_recv(struct state* state, char *buf, int len, int *msg_len, struct sockaddr * src_addr, socklen_t *src_addr_len)
{
    if ((*msg_len = recvfrom(state->sockfd, buf, len, 0, src_addr, src_addr_len)) == -1)
        return errno;

    return ERR_OK;
}

/* ************************************************************************** */

/**
 *  Tries to decode record. On success, function will return 0,
 *  ptr will point to the second element of the record and
 *  name and arity will be filled with record name and arity.
 */
int decode_record_hdr(char *buf, int len, int *ptr, char* name, int* arity)
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

    return ERR_OK;
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

    return ERR_OK;
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
    fprintf(stderr, "read_command: len=%d\n", len);
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
