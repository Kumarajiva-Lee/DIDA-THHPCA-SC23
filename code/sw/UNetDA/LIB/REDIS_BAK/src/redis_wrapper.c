#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hircluster.h>

void reply_returnf(redisReply *reply, float *buf, int *tag);
void reply_returnd(redisReply *reply, double *buf, int *tag);
void reply_returni(redisReply *reply, int *buf, int *tag);
void reply_check(redisReply *reply);

void get_ith_node(int index, char *nodes, char *node) {
    int len = strlen(nodes);
    int i;

    int nodes_cnt = 0;
    for (i = 0; i < len; i++)
        if (nodes[i] == ',')
            nodes_cnt++;

    index = index % nodes_cnt;

    i = 0;
    int split_cnt = 0;
    int s, e;
    while (split_cnt < index) {
        if (nodes[i] == ',')
            split_cnt++;
        i++;
    }
    s = i;
    while (split_cnt < index + 1 && i <= len) {
        if (nodes[i] == ',')
            split_cnt++;
        i++;
    }
    e = i - 2;
    strncpy(node, nodes + s, e - s + 1);
    node[e - s + 1] = '\0';
}

redisClusterContext* redisClusterConnectBalanced(char *nodes, int rank) {
    char node[50];
    get_ith_node(rank, nodes, node);
    return redisClusterConnect(node, 0);
}

void redis_sendall(redisClusterContext *cc) {
    int ret = redisCLusterSendAll(cc); 
    if (ret == REDIS_ERR) {
        printf("send all error\n");
        exit(-1);
    }
}

void trim_string(char *str)
{
    char *start, *end;
    int len = strlen(str);
 
    if(str[len-1] == '\n') {
        len--;
        str[len] = 0;
    }

    start = str;
    end = str + len -1;
    while(*start && isspace(*start))
        start++;
    while(*end && isspace(*end))
        *end-- = 0;
    strcpy(str, start);
}

redisClusterContext *setupConnection(char *redis_address)
{
    redisReply   *reply;

    redisClusterContext *cc;
    cc = redisClusterContextInit();
	cc->flags |= REDIS_BLOCK;
    redisClusterSetOptionAddNodes(cc, redis_address);
    redisClusterSetOptionRouteUseSlots(cc);
    redisClusterConnect2(cc);
    if (cc == NULL || cc->err) {
        if (cc) {
            printf("Connection error: %s\n", cc->errstr);
            redisClusterFree(cc);
        }
        else {
            printf("Connection error: can't allocate redis context\n");
        }
        exit(1);
    }

    return cc;
}

void redis_set(redisClusterContext *cc, char *key, char *value)
{
    redisReply *reply = redisClusterCommand(cc, "SET %s %s", key, value);
    reply_check(reply);
    freeReplyObject(reply);
}

void redis_setb(redisClusterContext *cc, char *key, void *value, int * len)
{
    redisReply *reply = redisClusterCommand(cc, "SET %s %b", key, value, *len);
    reply_check(reply);
    freeReplyObject(reply);
}

void redis_get(redisClusterContext *cc, char *key, void *value)
{
    redisReply *reply = redisClusterCommand(cc, "GET %s", key);
    switch(reply->type) {
        case 1 :
        case 3 :
            memcpy( value, reply->str, reply->len);
            break;
        case 4:
            printf("return null\n");
            break;
        case 5:
            printf("return status: %s\n", reply->str);
            break;
        case 6:
            printf("return error: %s\n", reply->str);
            break;
        default:
            printf("return no match (error)!\n");
    }
    freeReplyObject(reply);
}

void redis_cmd(redisClusterContext *cc, char *cmd)
{
    redisReply *reply = redisClusterCommand(cc, cmd);
    reply_check(reply);
    freeReplyObject(reply);
}

void redis_cmdreturnd(redisClusterContext *cc, char *cmd, double *buf)
{
    int i;
    int mm=0;
    redisReply *reply = redisClusterCommand(cc, cmd);
    reply_returnd(reply, buf, &mm);
    freeReplyObject(reply);
}

void redis_cmdreturnf(redisClusterContext *cc, char *cmd, float *buf)
{
    int i;
    int mm=0;
    redisReply *reply = redisClusterCommand(cc, cmd);
    reply_returnf(reply, buf, &mm);
    freeReplyObject(reply);
}

void redis_cmdreturni(redisClusterContext *cc, char *cmd, int *buf)
{
    int i;
    int mm=0;
    redisReply *reply = redisClusterCommand(cc, cmd);
    reply_returni(reply, buf, &mm); 
    freeReplyObject(reply);
}

void redisTestSet(redisClusterContext *c)
{
    printf("Attempting to GET wxsc\n");
    redisReply *reply = redisClusterCommand(c, "GET wxsc");
    printf("get value: %s\n", reply->str);
}

#ifdef BINARY

#define MAX_KEY_LEN 100
#define MAX_FIELD_LEN 20
// 修改为二进制接口
void RedisHseti(redisClusterContext *cc, char *hash, char *key, int *value)
{
    int argc = 4;
    char* argv[4];
    argv[0] = "hset";
    argv[1] = hash;
    argv[2] = key;
    argv[3] = (char *)value;

    size_t argvlen[4];
    argvlen[0] = 4;
    argvlen[1] = strlen(hash);
    argvlen[2] = strlen(key);
    argvlen[3] = sizeof(int);

    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_check(reply);
    freeReplyObject(reply);
}


// 修改为二进制接口
void RedisHgeti(redisClusterContext *cc, char *hash, char *key, int *value)
{
    int argc = 3;
    char* argv[3];
    argv[0] = "hget";
    argv[1] = hash;
    argv[2] = key;

    size_t argvlen[3];
    argvlen[0] = 4;
    argvlen[1] = strlen(hash);
    argvlen[2] = strlen(key);

    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_check(reply);
    switch(reply->type) {
        case 1 :
            memcpy(value, reply->str, sizeof(int));
            break;
        default:
            printf("hget return no match (error)!\n");
            exit(-1);
    }

    freeReplyObject(reply);
}

// 修改为二进制接口
void RedisHsetf(redisClusterContext *cc, char *hash, char *key, float *value)
{
    int argc = 4;
    char* argv[4];
    argv[0] = "hset";
    argv[1] = hash;
    argv[2] = key;
    argv[3] = (char *)value;

    size_t argvlen[4];
    argvlen[0] = 4;
    argvlen[1] = strlen(hash);
    argvlen[2] = strlen(key);
    argvlen[3] = sizeof(float);

    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_check(reply);
    freeReplyObject(reply);
}

// 修改为二进制接口
void RedisHgetf(redisClusterContext *cc, char *hash, char *key, float *value)
{
    int argc = 3;
    char* argv[3];
    argv[0] = "hget";
    argv[1] = hash;
    argv[2] = key;

    size_t argvlen[3];
    argvlen[0] = 4;
    argvlen[1] = strlen(hash);
    argvlen[2] = strlen(key);

    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_check(reply);
    switch(reply->type) {
        case 1 :
            memcpy(value, reply->str, sizeof(float));
            break;
        default:
            printf("hget return no match (error)!\n");
            exit(-1);
    }

    freeReplyObject(reply);
}

// 修改为二进制接口
void RedisHsetd(redisClusterContext *cc, char *hash, char *key, double *value)
{
    int argc = 4;
    char* argv[4];
    argv[0] = "hset";
    argv[1] = hash;
    argv[2] = key;
    argv[3] = (char *)value;

    size_t argvlen[4];
    argvlen[0] = 4;
    argvlen[1] = strlen(hash);
    argvlen[2] = strlen(key);
    argvlen[3] = sizeof(double);

    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_check(reply);
    freeReplyObject(reply);
}

void RedisHgetd(redisClusterContext *cc, char *hash, char *key, double *value)
{
    int argc = 3;
    char* argv[3];
    argv[0] = "hget";
    argv[1] = hash;
    argv[2] = key;

    size_t argvlen[3];
    argvlen[0] = 4;
    argvlen[1] = strlen(hash);
    argvlen[2] = strlen(key);

    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_check(reply);
    switch(reply->type) {
        case 1 :
            memcpy(value, reply->str, sizeof(double));
            break;
        default:
            printf("hget return no match (error)!\n");
            exit(-1);
    }

    freeReplyObject(reply);
}

char *RedisHsets(redisClusterContext *cc, char *hash, char *key, char *value)
{
    redisReply *reply = redisClusterCommand(cc, "HSET %s %s %s", hash, key, value);
    reply_check(reply);
    freeReplyObject(reply);
}

char *RedisHgets(redisClusterContext *cc, char *hash, char *key, char *value, int *len1, int *len2, int *len3)
{
    redisReply *reply = redisClusterCommand(cc, "HGET %s %s", hash, key);
    reply_check(reply);
    switch(reply->type) {
        case 1 :
            strcpy(value, reply->str);
            //*len3=reply->len;
            trim_string(value);
            *len3=strlen(value);
            break;
        default:
            printf("hget return no match (error)!\n");
            exit(-1);
    }

    freeReplyObject(reply);
}

void redis_hmsetf1d(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride, int *nn, float *buffer)
{
    int i, j, v;
    int n = *nn;
    int kv_cnt = ((*ite - *its) / *istride + 1) * n;

    int argc = kv_cnt * 2 + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));

    /*string split*/
    char var[n][32];
    char* token = strtok(varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

    argv[0] = "hmset";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for (i = *its; i <= *ite; i += *istride) {
        for (v = 0; v < n; v++) {
            sprintf(keys + offset, "%d:%s", i, var[v]);
            argv[pos * 2 + 2] = keys + offset;
            argvlen[pos * 2 + 2] = strlen(keys + offset);
            offset += argvlen[pos * 2 + 2];
            argv[pos * 2 + 3] = (char *)(buffer + ((i - *its) * n + v));
            argvlen[pos * 2 + 3] = sizeof(float);
            pos++;
        }
    }

    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_check(reply);

    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
}


void redis_hmgetf1d(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride, int *nn, float *buffer)
{
    int i, j, v;
    int n = *nn;
    int kv_cnt = ((*ite - *its) / *istride + 1) * n;

    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));

    /*string split*/
    char var[n][32];
    char* token = strtok(varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

    argv[0] = "hmget";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for (i = *its; i <= *ite; i += *istride) {
        for (v = 0; v < n; v++) {
            sprintf(keys + offset, "%d:%s", i, var[v]);
            argv[pos + 2] = keys + offset;
            argvlen[pos + 2] = strlen(keys + offset);
            offset += argvlen[pos + 2];
            pos++;
        }
    }

    int mm=0;
    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_returnf(reply, buffer, &mm); 

    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
} 


void redis_hmsetf2d(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, int *nn, float *buffer)
{
    int i, j, v;
    int n = *nn;
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_jn = sec_j * n;
    
    int i_cnt = (*ite - *its) / *istride + 1;
    int j_cnt = (*jte - *jts) / *jstride + 1;
    int jn_cnt = j_cnt * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = jn_cnt;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));

    for(i = *its; i <= *ite; i += *istride) {
        argv[0] = "hmset";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *jts; j <= *jte; j += *jstride) {
            for(v =0; v<n; v++) {
                sprintf(keys + offset, "%d:0:%s", j, var[v]);
                argv[pos * 2 + 2] = keys + offset;
                argvlen[pos * 2 + 2] = strlen(keys + offset);
                offset += argvlen[pos * 2 + 2];
                argv[pos * 2 + 3] = (char *)(buffer+((i - *its) * sec_jn
                                            + (j - *jts) * n + v));
                argvlen[pos * 2 + 3] = sizeof(float);
                pos++;
            }
        }
        int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }


    for(i = *its; i <= *ite; i += *istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);
        freeReplyObject(reply);
    }
    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    int kv_cnt = i_cnt * jn_cnt;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));

    argv[0] = "hmset";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        for(j = *jts; j <= *jte; j += *jstride) {
            for(v = 0; v < n; v++) {
                sprintf(keys + offset, "%d:%d:0:%s", i, j, var[v]);
                argv[pos * 2 + 2] = keys + offset;
                argvlen[pos * 2 + 2] = strlen(keys + offset);
                offset += argvlen[pos * 2 + 2];
                argv[pos * 2 + 3] = (char *)(buffer+((i - *its) * sec_jn
                                            + (j - *jts) * n + v));
                argvlen[pos * 2 + 3] = sizeof(float);
                pos++;
            }
        }
    }

    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_check(reply);

    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
#endif
}

void redis_hmgetf2d(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, int *nn, float *buffer)
{
    int i, j, v;
    int n = *nn;
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_jn = sec_j * n;
    
    int i_cnt = (*ite - *its) / *istride + 1;
    int j_cnt = (*jte - *jts) / *jstride + 1;
    int jn_cnt = j_cnt * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = jn_cnt;
    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];
  
    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
	
    for(i = *its; i <= *ite; i += *istride) {
        argv[0] = "hmget";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *jts; j <= *jte; j += *jstride) {
            for(v = 0; v < n; v++) {
                sprintf(keys + offset, "%d:0:%s", j, var[v]);
                argv[pos + 2] = keys + offset;
                argvlen[pos + 2] = strlen(keys + offset);
                offset += argvlen[pos + 2];
                pos++;
            }
        }
		int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }

    int mm = 0;
    for(i = *its; i <= *ite; i += *istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_returnf(reply, buffer, &mm);
        freeReplyObject(reply);
    }

    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    int kv_cnt = i_cnt * jn_cnt;
    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));

    argv[0] = "hmget";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        for(j = *jts; j <= *jte; j += *jstride) {
            for(v=0; v<n; v++) {
                sprintf(keys + offset, "%d:%d:0:%s", i, j, var[v]);
                argv[pos + 2] = keys + offset;
                argvlen[pos + 2] = strlen(keys + offset);
                offset += argvlen[pos + 2];
                pos++;
            }
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_returnf(reply, buffer, &mm);

    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
#endif
}

void redis_hmsetf(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride,
                  int *jts, int *jte, int *jstride, int *kts, int *kte, int *kstride, int *nn, float *buffer)
{
    int i, j, k, v;
    int n = *nn;
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_k = *kte - *kts + 1;
    int sec_jkn = sec_j * sec_k * n;
    int sec_kn = sec_k * n;
    
    int i_cnt = (*ite - *its) / *istride + 1;
    int j_cnt = (*jte - *jts) / *jstride + 1;
    int k_cnt = (*kte - *kts) / *kstride + 1;
    int jkn_cnt = j_cnt * k_cnt * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = jkn_cnt;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        argv[0] = "hmset";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *jts; j <= *jte; j += *jstride) {
            for (k = *kts; k <= *kte; k += *kstride) {
                for(v = 0; v < n; v++) {
                    sprintf(keys + offset, "%d:%d:%s", j, k, var[v]);
                    argv[pos * 2 + 2] = keys + offset;
                    argvlen[pos * 2 + 2] = strlen(keys + offset);
                    offset += argvlen[pos * 2 + 2];
                    argv[pos * 2 + 3] = (char *)(buffer+((i - *its) * sec_jkn
                                                + (j - *jts) * sec_kn 
                                                + (k - *kts) * n
                                                + v));
                    argvlen[pos * 2 + 3] = sizeof(float);
                    pos++;
                }
            }
        }
        int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }

    for(i = *its; i <= *ite; i += *istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);
        freeReplyObject(reply);
    }

    free(argv);
    free(argvlen);
    free(keys);
#else
    int kv_cnt = i_cnt * jkn_cnt;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));

    argv[0] = "hmset";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        for(j = *jts; j <= *jte; j += *jstride) {
            for (k = *kts; k <= *kte; k += *kstride) {
                for(v = 0; v < n; v++) {
                    sprintf(keys + offset, "%d:%d:%d:%s", i, j, k, var[v]);
                    argv[pos * 2 + 2] = keys + offset;
                    argvlen[pos * 2 + 2] = strlen(keys + offset);
                    offset += argvlen[pos * 2 + 2];
                    argv[pos * 2 + 3] = (char *)(buffer+((i - *its) * sec_jkn
                                                + (j - *jts) * sec_kn 
                                                + (k - *kts) * n
                                                + v));
                    argvlen[pos * 2 + 3] = sizeof(float);
                    pos++;
                }
            }
        }
    }

    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_check(reply);

    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
#endif
}

void redis_hmgetf(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride,
                  int *jts, int *jte, int *jstride, int *kts, int *kte, int *kstride, int *nn, float *buffer)
{
    int i, j, k, v;
    int n = *nn;
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_k = *kte - *kts + 1;
    int sec_jkn = sec_j * sec_k * n;
    int sec_kn = sec_k * n;
    
    int i_cnt = (*ite - *its) / *istride + 1;
    int j_cnt = (*jte - *jts) / *jstride + 1;
    int k_cnt = (*kte - *kts) / *kstride + 1;
    int jkn_cnt = j_cnt * k_cnt * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = jkn_cnt;
    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));

    for(i = *its; i <= *ite; i += *istride) {
        argv[0] = "hmget";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
		offset = 0;
        for(j = *jts; j <= *jte; j += *jstride) {
            for (k = *kts; k <= *kte; k += *kstride) {
                for(v = 0; v < n; v++) {
                    sprintf(keys + offset, "%d:%d:%s", j, k, var[v]);
                    argv[pos + 2] = keys + offset;
                    argvlen[pos + 2] = strlen(keys + offset);
                    offset += argvlen[pos + 2];
                    pos++;
                }
            }
        }
        int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }

    int mm = 0;
    for(i = *its; i <= *ite; i += *istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_returnf(reply, buffer, &mm);
        freeReplyObject(reply);
    }

    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    int kv_cnt = i_cnt * jkn_cnt;
    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));

    argv[0] = "hmget";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        for(j = *jts; j <= *jte; j += *jstride) {
            for(k = *kts; k <= *kte; k += *kstride) {
                for(v = 0; v < n; v++) {
                    sprintf(keys + offset, "%d:%d:%d:%s", i, j, k, var[v]);
                    argv[pos + 2] = keys + offset;
                    argvlen[pos + 2] = strlen(keys + offset);
                    offset += argvlen[pos + 2];
                    pos++;
                }
            }
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_returnf(reply, buffer, &mm);

    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
#endif
}

void redis_hmsetd1d(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride, int *nn, double *buffer)
{
    int i, j, v;
    int n = *nn;
    int kv_cnt = ((*ite - *its) / *istride + 1) * n;

    int argc = kv_cnt * 2 + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));

    /*string split*/
    char var[n][32];
    char* token = strtok(varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

    argv[0] = "hmset";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for (i = *its; i <= *ite; i += *istride) {
        for (v = 0; v < n; v++) {
            sprintf(keys + offset, "%d:%s", i, var[v]);
            argv[pos * 2 + 2] = keys + offset;
            argvlen[pos * 2 + 2] = strlen(keys + offset);
            offset += argvlen[pos * 2 + 2];
            argv[pos * 2 + 3] = (char *)(buffer + ((i - *its) * n + v));
            argvlen[pos * 2 + 3] = sizeof(double);
            pos++;
        }
    }

    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_check(reply);

    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
}

void redis_hmgetd1d(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride, int *nn, double *buffer)
{
    int i, j, v;
    int n = *nn;
    int kv_cnt = ((*ite - *its) / *istride + 1) * n;

    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));

    /*string split*/
    char var[n][32];
    char* token = strtok(varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

    argv[0] = "hmget";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for (i = *its; i <= *ite; i += *istride) {
        for (v = 0; v < n; v++) {
            sprintf(keys + offset, "%d:%s", i, var[v]);
            argv[pos + 2] = keys + offset;
            argvlen[pos + 2] = strlen(keys + offset);
            offset += argvlen[pos + 2];
            pos++;
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_returnd(reply, buffer, &mm);

    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
} 


void redis_hmsetd2d(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, int *nn, double *buffer)
{
    int i, j, v;
    int n = *nn;
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_jn = sec_j * n;
    
    int i_cnt = (*ite - *its) / *istride + 1;
    int j_cnt = (*jte - *jts) / *jstride + 1;
    int jn_cnt = j_cnt * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = jn_cnt;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));

    for(i = *its; i <= *ite; i += *istride) {
        argv[0] = "hmset";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *jts; j <= *jte; j += *jstride) {
            for(v =0; v<n; v++) {
                sprintf(keys + offset, "%d:0:%s", j, var[v]);
                argv[pos * 2 + 2] = keys + offset;
                argvlen[pos * 2 + 2] = strlen(keys + offset);
                offset += argvlen[pos * 2 + 2];
                argv[pos * 2 + 3] = (char *)(buffer+((i - *its) * sec_jn
                                            + (j - *jts) * n + v));
                argvlen[pos * 2 + 3] = sizeof(double);
                pos++;
            }
        }
        int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }

    for(i = *its; i <= *ite; i += *istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);

        freeReplyObject(reply);
    }
    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    int kv_cnt = i_cnt * jn_cnt;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));

    argv[0] = "hmset";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        for(j = *jts; j <= *jte; j += *jstride) {
            for(v = 0; v < n; v++) {
                sprintf(keys + offset, "%d:%d:0:%s", i, j, var[v]);
                argv[pos * 2 + 2] = keys + offset;
                argvlen[pos * 2 + 2] = strlen(keys + offset);
                offset += argvlen[pos * 2 + 2];
                argv[pos * 2 + 3] = (char *)(buffer+((i - *its) * sec_jn
                                            + (j - *jts) * n + v));
                argvlen[pos * 2 + 3] = sizeof(double);
                pos++;
            }
        }
    }

    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_check(reply);

    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
#endif
}

void redis_hmgetd2d(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, int *nn, double *buffer)
{
    int i, j, v;
    int n = *nn;
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_jn = sec_j * n;
    
    int i_cnt = (*ite - *its) / *istride + 1;
    int j_cnt = (*jte - *jts) / *jstride + 1;
    int jn_cnt = j_cnt * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = jn_cnt;
    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];
  
    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        argv[0] = "hmget";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *jts; j <= *jte; j += *jstride) {
            for(v = 0; v < n; v++) {
                sprintf(keys + offset, "%d:0:%s", j, var[v]);
                argv[pos + 2] = keys + offset;
                argvlen[pos + 2] = strlen(keys + offset);
                offset += argvlen[pos + 2];
                pos++;
            }
        }
        int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }

    int mm = 0;
    for(i = *its; i <= *ite; i += *istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_returnd(reply, buffer, &mm);
        freeReplyObject(reply);
    }
    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    int kv_cnt = i_cnt * jn_cnt;
    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));

    argv[0] = "hmget";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        for(j = *jts; j <= *jte; j += *jstride) {
            for(v=0; v<n; v++) {
                sprintf(keys + offset, "%d:%d:0:%s", i, j, var[v]);
                argv[pos + 2] = keys + offset;
                argvlen[pos + 2] = strlen(keys + offset);
                offset += argvlen[pos + 2];
                pos++;
            }
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_returnd(reply, buffer, &mm);
    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
#endif
}

void redis_hmsetd(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride,
                  int *jts, int *jte, int *jstride, int *kts, int *kte, int *kstride, int *nn, double *buffer)
{
    int i, j, k, v;
    int n = *nn;
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_k = *kte - *kts + 1;
    int sec_jkn = sec_j * sec_k * n;
    int sec_kn = sec_k * n;
    
    int i_cnt = (*ite - *its) / *istride + 1;
    int j_cnt = (*jte - *jts) / *jstride + 1;
    int k_cnt = (*kte - *kts) / *kstride + 1;
    int jkn_cnt = j_cnt * k_cnt * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = jkn_cnt;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        argv[0] = "hmset";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *jts; j <= *jte; j += *jstride) {
            for (k = *kts; k <= *kte; k += *kstride) {
                for(v = 0; v < n; v++) {
                    sprintf(keys + offset, "%d:%d:%s", j, k, var[v]);
                    argv[pos * 2 + 2] = keys + offset;
                    argvlen[pos * 2 + 2] = strlen(keys + offset);
                    offset += argvlen[pos * 2 + 2];
                    argv[pos * 2 + 3] = (char *)(buffer+((i - *its) * sec_jkn
                                                + (j - *jts) * sec_kn 
                                                + (k - *kts) * n
                                                + v));
                    argvlen[pos * 2 + 3] = sizeof(double);
                    pos++;
                }
            }
        }
        int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }

    for(i = *its; i <= *ite; i += *istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);

        freeReplyObject(reply);
    }

    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    int kv_cnt = i_cnt * jkn_cnt;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));

    argv[0] = "hmset";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        for(j = *jts; j <= *jte; j += *jstride) {
            for (k = *kts; k <= *kte; k += *kstride) {
                for(v = 0; v < n; v++) {
                    sprintf(keys + offset, "%d:%d:%d:%s", i, j, k, var[v]);
                    argv[pos * 2 + 2] = keys + offset;
                    argvlen[pos * 2 + 2] = strlen(keys + offset);
                    offset += argvlen[pos * 2 + 2];
                    argv[pos * 2 + 3] = (char *)(buffer+((i - *its) * sec_jkn
                                                + (j - *jts) * sec_kn 
                                                + (k - *kts) * n
                                                + v));
                    argvlen[pos * 2 + 3] = sizeof(double);
                    pos++;
                }
            }
        }
    }

    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_check(reply);

    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
#endif
}

void redis_hmgetd(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride,
                  int *jts, int *jte, int *jstride, int *kts, int *kte, int *kstride, int *nn, double *buffer)
{
    int i, j, k, v;
    int n = *nn;
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_k = *kte - *kts + 1;
    int sec_jkn = sec_j * sec_k * n;
    int sec_kn = sec_k * n;
    
    int i_cnt = (*ite - *its) / *istride + 1;
    int j_cnt = (*jte - *jts) / *jstride + 1;
    int k_cnt = (*kte - *kts) / *kstride + 1;
    int jkn_cnt = j_cnt * k_cnt * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = jkn_cnt;
    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));

    for(i = *its; i <= *ite; i += *istride) {
        argv[0] = "hmget";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *jts; j <= *jte; j += *jstride) {
            for (k = *kts; k <= *kte; k += *kstride) {
                for(v = 0; v < n; v++) {
                    sprintf(keys + offset, "%d:%d:%s", j, k, var[v]);
                    argv[pos + 2] = keys + offset;
                    argvlen[pos + 2] = strlen(keys + offset);
                    offset += argvlen[pos + 2];
                    pos++;
                }
            }
        }
        int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }

    int mm = 0;
    for(i = *its; i <= *ite; i += *istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_returnd(reply, buffer, &mm);
        freeReplyObject(reply);
    }

    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    int kv_cnt = i_cnt * jkn_cnt;
    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));

    argv[0] = "hmget";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        for(j = *jts; j <= *jte; j += *jstride) {
            for(k = *kts; k <= *kte; k += *kstride) {
                for(v = 0; v < n; v++) {
                    sprintf(keys + offset, "%d:%d:%d:%s", i, j, k, var[v]);
                    argv[pos + 2] = keys + offset;
                    argvlen[pos + 2] = strlen(keys + offset);
                    offset += argvlen[pos + 2];
                    pos++;
                }
            }
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_returnd(reply, buffer, &mm);
            
    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
#endif
}

void redis_da_outputf(redisClusterContext *cc, char *hkey, char *varlist, int *lonids, int *lonide, int *lonstep, 
                      int *latids, int *latide, int *latstep, int *mpas_num_lev_start, int *mpas_num_lev, int *zstep, 
                      int *num_2d, int *num_3d, float *buffer)
{
    int i, j, k, v;
    int n = (*num_2d) + (*num_3d) * (*mpas_num_lev) ;
    int sec_i = *lonide - *lonids + 1;
    int sec_j = *latide - *latids + 1;
    int sec_kjn = sec_j * n;

    int i_cnt = (*lonide - *lonids) / *lonstep + 1;
    int j_cnt = (*latide - *latids) / *latstep + 1;
    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = j_cnt * n;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char*));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *lonids; i <= *lonide; i += *lonstep) {
        argv[0] = "hmset";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *latids; j <= *latide; j += *latstep) {
            for(v = 0; v < *num_2d; v++) {
                sprintf(keys + offset, "%d:0:%s", j, var[v]);
                argv[2 * pos + 2] = keys + offset;
                argvlen[2 * pos + 2] = strlen(keys + offset);
                offset += argvlen[2 * pos + 2];
                argv[2 * pos + 3] = (char *)(buffer+((i - *lonids) * sec_kjn
                                    + (j - *latids) * n + v));
                argvlen[2 * pos + 3] = sizeof(float);
                pos++;
            }
            for(v = 0; v < *num_3d; v++) {
                for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k += *zstep) {
                    sprintf(keys + offset, "%d:%d:%s", j, k, var[*num_2d + v]);
                    argv[2 * pos + 2] = keys + offset;
                    argvlen[2 * pos + 2] = strlen(keys + offset);
                    offset += argvlen[2 * pos + 2];
                    argv[2 * pos + 3] = (char *)(buffer + ((i - *lonids) * sec_kjn
                                        +(j - *latids) * n
                                        +(*num_2d + v * (*mpas_num_lev)) + k - 1));
                    argvlen[2 * pos + 3] = sizeof(float);
                    pos++;
                }
            }
        }
        redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    }

    for(i = *lonids; i <= *lonide; i += *lonstep) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);

        freeReplyObject(reply);
    }
    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    if (n * sec_i * sec_j < 518400) {
        int kv_cnt = i_cnt * j_cnt * n;
        int argc = 2 * kv_cnt + 2;
        char** argv = calloc(argc, sizeof(char *));
        size_t* argvlen = calloc(argc, sizeof(size_t));

        argv[0] = "hmset";
        argvlen[0] = 5;
        argv[1] = hkey;
        argvlen[1] = strlen(hkey);

        int pos = 0, offset = 0;
        char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
        for(i = *lonids; i <= *lonide; i += *lonstep) {
            for(j = *latids; j <= *latide; j += *latstep) {
                for(v = 0; v < *num_2d; v++) {
                    sprintf(keys + offset, "%d:%d:0:%s", i, j, var[v]);
                    argv[2 * pos + 2] = keys + offset;
                    argvlen[2 * pos + 2] = strlen(keys + offset);
                    offset += argvlen[2 * pos + 2];
                    argv[2 * pos + 3] = (char *)(buffer+((i - *lonids) * sec_kjn
                                        + (j - *latids) * n + v));
                    argvlen[2 * pos + 3] = sizeof(float);
                    pos++;
                }
                for(v = 0; v < *num_3d; v++) {
                    for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k += *zstep) {
                        sprintf(keys + offset, "%d:%d:%d:%s", i, j, k, var[*num_2d + v]);
                        argv[2 * pos + 2] = keys + offset;
                        argvlen[2 * pos + 2] = strlen(keys + offset);
                        offset += argvlen[2 * pos + 2];
                        argv[2 * pos + 3] = (char *)(buffer+((i - *lonids) * sec_kjn
                                            +(j - *latids) * n
                                            +(*num_2d + v*(*mpas_num_lev)) + k - 1));
                        argvlen[2 * pos + 3] = sizeof(float);
                        pos++;
                    }
                }
            }
        }

        redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
        reply_check(reply);

        free(argv);
        free(argvlen);
        free(keys);
        freeReplyObject(reply);
    } else {
        redisReply *reply;
        int kv_cnt = i_cnt * n;
        int argc = 2 * kv_cnt + 2;
        char** argv = calloc(argc, sizeof(char *));
        size_t* argvlen = calloc(argc, sizeof(size_t));
        
        int pos = 0, offset = 0;
        char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
        for(j = *latids; j <= *latide; j += *latstep) {
            argv[0] = "hmset";
            argvlen[0] = 5;
            argv[1] = hkey;
            argvlen[1] = strlen(hkey);

            pos = 0;
            offset = 0;
            for(i = *lonids; i <= *lonide; i += *lonstep) {
                for(v=0; v<*num_2d; v++) {
                    sprintf(keys + offset, "%d:%d:0:%s", i, j, var[v]);
                    argv[2 * pos + 2] = keys + offset;
                    argvlen[2 * pos + 2] = strlen(keys + offset);
                    offset += argvlen[2 * pos + 2];
                    argv[2 * pos + 3] = (char *)(buffer+((i - *lonids) * sec_kjn
                                        + (j - *latids) * n + v));
                    argvlen[2 * pos + 3] = sizeof(float);
                    pos++;
                }
                for(v = 0; v < *num_3d; v++) {
                    for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                        sprintf(keys + offset, "%d:%d:%d:%s", i, j, k, var[*num_2d + v]);
                        argv[2 * pos + 2] = keys + offset;
                        argvlen[2 * pos + 2] = strlen(keys + offset);
                        offset += argvlen[2 * pos + 2];
                        argv[2 * pos + 3] = (char *)(buffer+((i - *lonids) * sec_kjn
                                            +(j - *latids) * n
                                            +(*num_2d + v*(*mpas_num_lev)) + k - 1));
                        argvlen[2 * pos + 3] = sizeof(float);
                        pos++;
                    }
                }
            }
            redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
        }

        for(j = *latids; j <= *latide; j+=*latstep) {
            int r = redisClusterGetReply(cc, (void **) &reply);
            if (r == REDIS_ERR) { printf("Generic Redis Reply Error\n"); exit(-1);  }
            reply_check(reply);

            freeReplyObject(reply);
        }
        free(argv);
        free(argvlen);
        free(keys);
    }
#endif
}

void redis_da_outputd(redisClusterContext *cc, char *hkey, char *varlist, int *lonids, int *lonide, int *lonstep, 
                      int *latids, int *latide, int *latstep, int *mpas_num_lev_start, int *mpas_num_lev, int *zstep, 
                      int *num_2d, int *num_3d, double *buffer)
{
    int i, j, k, v;
    int n = (*num_2d) + (*num_3d) * (*mpas_num_lev) ;
    int sec_i = *lonide - *lonids + 1;
    int sec_j = *latide - *latids + 1;
    int sec_kjn = sec_j * n;

    int i_cnt = (*lonide - *lonids) / *lonstep + 1;
    int j_cnt = (*latide - *latids) / *latstep + 1;
    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = j_cnt * n;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char*));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *lonids; i <= *lonide; i += *lonstep) {
        argv[0] = "hmset";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *latids; j <= *latide; j += *latstep) {
            for(v = 0; v < *num_2d; v++) {
                sprintf(keys + offset, "%d:0:%s", j, var[v]);
                argv[2 * pos + 2] = keys + offset;
                argvlen[2 * pos + 2] = strlen(keys + offset);
                offset += argvlen[2 * pos + 2];
                argv[2 * pos + 3] = (char *)(buffer+((i - *lonids) * sec_kjn
                                    + (j - *latids) * n + v));
                argvlen[2 * pos + 3] = sizeof(double);
                pos++;
            }
            for(v = 0; v < *num_3d; v++) {
                for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k += *zstep) {
                    sprintf(keys + offset, "%d:%d:%s", j, k, var[*num_2d + v]);
                    argv[2 * pos + 2] = keys + offset;
                    argvlen[2 * pos + 2] = strlen(keys + offset);
                    offset += argvlen[2 * pos + 2];
                    argv[2 * pos + 3] = (char *)(buffer + ((i - *lonids) * sec_kjn
                                        + (j - *latids) * n
                                        + (*num_2d + v * (*mpas_num_lev)) + k - 1));
                    argvlen[2 * pos + 3] = sizeof(double);
                    pos++;
                }
            }
        }
        redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    }

    for(i = *lonids; i <= *lonide; i += *lonstep) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);

        freeReplyObject(reply);
    }
    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    if (n * sec_i * sec_j < 518400) {
        int kv_cnt = i_cnt * j_cnt * n;
        int argc = 2 * kv_cnt + 2;
        char** argv = calloc(argc, sizeof(char *));
        size_t* argvlen = calloc(argc, sizeof(size_t));

        argv[0] = "hmset";
        argvlen[0] = 5;
        argv[1] = hkey;
        argvlen[1] = strlen(hkey);

        int pos = 0, offset = 0;
        char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
        for(i = *lonids; i <= *lonide; i += *lonstep) {
            for(j = *latids; j <= *latide; j += *latstep) {
                for(v = 0; v < *num_2d; v++) {
                    sprintf(keys + offset, "%d:%d:0:%s", i, j, var[v]);
                    argv[2 * pos + 2] = keys + offset;
                    argvlen[2 * pos + 2] = strlen(keys + offset);
                    offset += argvlen[2 * pos + 2];
                    argv[2 * pos + 3] = (char *)(buffer+((i - *lonids) * sec_kjn
                                        + (j - *latids) * n + v));
                    argvlen[2 * pos + 3] = sizeof(double);
                    pos++;
                }
                for(v=0; v<*num_3d; v++) {
                    for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k += *zstep) {
                        sprintf(keys + offset, "%d:%d:%d:%s", i, j, k, var[*num_2d + v]);
                        argv[2 * pos + 2] = keys + offset;
                        argvlen[2 * pos + 2] = strlen(keys + offset);
                        offset += argvlen[2 * pos + 2];
                        argv[2 * pos + 3] = (char *)(buffer+((i - *lonids) * sec_kjn
                                            +(j - *latids) * n
                                            +(*num_2d + v*(*mpas_num_lev)) + k - 1));
                        argvlen[2 * pos + 3] = sizeof(double);
                        pos++;
                    }
                }
            }
        }

        redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
        reply_check(reply);

        free(argv);
        free(argvlen);
        free(keys);
        freeReplyObject(reply);
    } else {
        redisReply *reply;
        int kv_cnt = i_cnt * n;
        int argc = 2 * kv_cnt + 2;
        char** argv = calloc(argc, sizeof(char *));
        size_t* argvlen = calloc(argc, sizeof(size_t));
        
        int pos = 0, offset = 0;
        char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
        for(j = *latids; j <= *latide; j += *latstep) {
            argv[0] = "hmset";
            argvlen[0] = 5;
            argv[1] = hkey;
            argvlen[1] = strlen(hkey);

            pos = 0;
            offset = 0;
            for(i = *lonids; i <= *lonide; i += *lonstep) {
                for(v = 0; v < *num_2d; v++) {
                    sprintf(keys + offset, "%d:%d:0:%s", i, j, var[v]);
                    argv[2 * pos + 2] = keys + offset;
                    argvlen[2 * pos + 2] = strlen(keys + offset);
                    offset += argvlen[2 * pos + 2];
                    argv[2 * pos + 3] = (char *)(buffer + ((i - *lonids) * sec_kjn
                                        + (j - *latids) * n 
                                        + v));
                    argvlen[2 * pos + 3] = sizeof(double);
                    pos++;
                }
                for(v = 0; v < *num_3d; v++) {
                    for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k += *zstep) {
                        sprintf(keys + offset, "%d:%d:%d:%s", i, j, k, var[*num_2d + v]);
                        argv[2 * pos + 2] = keys + offset;
                        argvlen[2 * pos + 2] = strlen(keys + offset);
                        offset += argvlen[2 * pos + 2];
                        argv[2 * pos + 3] = (char *)(buffer + ((i - *lonids) * sec_kjn
                                            +(j - *latids) * n
                                            +(*num_2d + v*(*mpas_num_lev)) + k - 1));
                        argvlen[2 * pos + 3] = sizeof(double);
                        pos++;
                    }
                }
            }
            redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
        }

        for(j = *latids; j <= *latide; j += *latstep) {
            int r = redisClusterGetReply(cc, (void **) &reply);
            if (r == REDIS_ERR) { printf("Generic Redis Reply Error\n"); exit(-1);  }
            reply_check(reply);

            freeReplyObject(reply);
        }
        free(argv);
        free(argvlen);
        free(keys);
    }
#endif
}


//-------------------------------------------------
// non-block interfaces
//-------------------------------------------------



void redis_hmsetf1d_nonblock(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride, int *nn, float *buffer)
{
    int i, j, v;
    int n = *nn;
    int kv_cnt = ((*ite - *its) / *istride + 1) * n;

    int argc = kv_cnt * 2 + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));

    /*string split*/
    char var[n][32];
    char* token = strtok(varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

    argv[0] = "hmset";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for (i = *its; i <= *ite; i += *istride) {
        for (v = 0; v < n; v++) {
            sprintf(keys + offset, "%d:%s", i, var[v]);
            argv[pos * 2 + 2] = keys + offset;
            argvlen[pos * 2 + 2] = strlen(keys + offset);
            offset += argvlen[pos * 2 + 2];
            argv[pos * 2 + 3] = (char *)(buffer + ((i - *its) * n + v));
            argvlen[pos * 2 + 3] = sizeof(float);
            pos++;
        }
    }

    int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    if (ret == REDIS_ERR) {
        printf("append command error\n");
        exit(-1);
    }

    free(argv);
    free(argvlen);
    free(keys);
}

void redis_hmsetf1d_nonblock_get_reply(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride, int *nn, float *buffer) {
    void *reply;
    int status = redisClusterGetReply(cc, &reply);
    if (status == REDIS_OK) {
        reply_check(reply);
        freeReplyObject(reply);
    }
    else {
        printf("redis_hmsetf1d_nonblock_get_reply error!\n");
        exit(-1);
    }
}

void redis_hmgetf1d_nonblock(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride, int *nn, float *buffer)
{
    int i, j, v;
    int n = *nn;
    int kv_cnt = ((*ite - *its) / *istride + 1) * n;

    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));

    /*string split*/
    char var[n][32];
    char* token = strtok(varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

    argv[0] = "hmget";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for (i = *its; i <= *ite; i += *istride) {
        for (v = 0; v < n; v++) {
            sprintf(keys + offset, "%d:%s", i, var[v]);
            argv[pos + 2] = keys + offset;
            argvlen[pos + 2] = strlen(keys + offset);
            offset += argvlen[pos + 2];
            pos++;
        }
    }

    int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    if (ret == REDIS_ERR) {
        printf("append command error\n");
        exit(-1);
    }

    free(argv);
    free(argvlen);
    free(keys);
} 

void redis_hmgetf1d_nonblock_get_reply(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride, int *nn, float *buffer) {
    void *reply;
    int status = redisClusterGetReply(cc, &reply);
    if (status == REDIS_OK) {
        int mm = 0;        
        reply_returnf(reply, buffer, &mm);
        freeReplyObject(reply);
    }
    else {
        printf("redis_hmsetf1d_nonblock_get_reply error!\n");
        exit(-1);
    }
}

void redis_hmsetf2d_nonblock(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, int *nn, float *buffer)
{
    int i, j, v;
    int n = *nn;
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_jn = sec_j * n;
    
    int i_cnt = (*ite - *its) / *istride + 1;
    int j_cnt = (*jte - *jts) / *jstride + 1;
    int jn_cnt = j_cnt * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = jn_cnt;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));

    for(i = *its; i <= *ite; i += *istride) {
        argv[0] = "hmset";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *jts; j <= *jte; j += *jstride) {
            for(v =0; v<n; v++) {
                sprintf(keys + offset, "%d:0:%s", j, var[v]);
                argv[pos * 2 + 2] = keys + offset;
                argvlen[pos * 2 + 2] = strlen(keys + offset);
                offset += argvlen[pos * 2 + 2];
                argv[pos * 2 + 3] = (char *)(buffer+((i - *its) * sec_jn
                                            + (j - *jts) * n + v));
                argvlen[pos * 2 + 3] = sizeof(float);
                pos++;
            }
        }
        int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }

    redis_sendall(cc);

    for(i = *its; i <= *ite; i += *istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);
        freeReplyObject(reply);
    }
    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    int kv_cnt = i_cnt * jn_cnt;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));

    argv[0] = "hmset";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        for(j = *jts; j <= *jte; j += *jstride) {
            for(v = 0; v < n; v++) {
                sprintf(keys + offset, "%d:%d:0:%s", i, j, var[v]);
                argv[pos * 2 + 2] = keys + offset;
                argvlen[pos * 2 + 2] = strlen(keys + offset);
                offset += argvlen[pos * 2 + 2];
                argv[pos * 2 + 3] = (char *)(buffer+((i - *its) * sec_jn
                                            + (j - *jts) * n + v));
                argvlen[pos * 2 + 3] = sizeof(float);
                pos++;
            }
        }
    }

    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_check(reply);

    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
#endif
}

void redis_hmgetf2d_nonblock(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, int *nn, float *buffer)
{
    int i, j, v;
    int n = *nn;
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_jn = sec_j * n;
    
    int i_cnt = (*ite - *its) / *istride + 1;
    int j_cnt = (*jte - *jts) / *jstride + 1;
    int jn_cnt = j_cnt * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = jn_cnt;
    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];
  
    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
	
    for(i = *its; i <= *ite; i += *istride) {
        argv[0] = "hmget";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *jts; j <= *jte; j += *jstride) {
            for(v = 0; v < n; v++) {
                sprintf(keys + offset, "%d:0:%s", j, var[v]);
                argv[pos + 2] = keys + offset;
                argvlen[pos + 2] = strlen(keys + offset);
                offset += argvlen[pos + 2];
                pos++;
            }
        }
		int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }

	redis_sendall(cc);

    int mm = 0;
    for(i = *its; i <= *ite; i += *istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_returnf(reply, buffer, &mm);
        freeReplyObject(reply);
    }

    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    int kv_cnt = i_cnt * jn_cnt;
    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));

    argv[0] = "hmget";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        for(j = *jts; j <= *jte; j += *jstride) {
            for(v=0; v<n; v++) {
                sprintf(keys + offset, "%d:%d:0:%s", i, j, var[v]);
                argv[pos + 2] = keys + offset;
                argvlen[pos + 2] = strlen(keys + offset);
                offset += argvlen[pos + 2];
                pos++;
            }
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_returnf(reply, buffer, &mm);

    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
#endif
}

void redis_hmsetf_nonblock(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride,
                  int *jts, int *jte, int *jstride, int *kts, int *kte, int *kstride, int *nn, float *buffer)
{
    int i, j, k, v;
    int n = *nn;
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_k = *kte - *kts + 1;
    int sec_jkn = sec_j * sec_k * n;
    int sec_kn = sec_k * n;
    
    int i_cnt = (*ite - *its) / *istride + 1;
    int j_cnt = (*jte - *jts) / *jstride + 1;
    int k_cnt = (*kte - *kts) / *kstride + 1;
    int jkn_cnt = j_cnt * k_cnt * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = jkn_cnt;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        argv[0] = "hmset";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *jts; j <= *jte; j += *jstride) {
            for (k = *kts; k <= *kte; k += *kstride) {
                for(v = 0; v < n; v++) {
                    sprintf(keys + offset, "%d:%d:%s", j, k, var[v]);
                    argv[pos * 2 + 2] = keys + offset;
                    argvlen[pos * 2 + 2] = strlen(keys + offset);
                    offset += argvlen[pos * 2 + 2];
                    argv[pos * 2 + 3] = (char *)(buffer+((i - *its) * sec_jkn
                                                + (j - *jts) * sec_kn 
                                                + (k - *kts) * n
                                                + v));
                    argvlen[pos * 2 + 3] = sizeof(float);
                    pos++;
                }
            }
        }
        int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }

    redis_sendall(cc);

    for(i = *its; i <= *ite; i += *istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);
        freeReplyObject(reply);
    }

    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    int kv_cnt = i_cnt * jkn_cnt;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));

    argv[0] = "hmset";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        for(j = *jts; j <= *jte; j += *jstride) {
            for (k = *kts; k <= *kte; k += *kstride) {
                for(v = 0; v < n; v++) {
                    sprintf(keys + offset, "%d:%d:%d:%s", i, j, k, var[v]);
                    argv[pos * 2 + 2] = keys + offset;
                    argvlen[pos * 2 + 2] = strlen(keys + offset);
                    offset += argvlen[pos * 2 + 2];
                    argv[pos * 2 + 3] = (char *)(buffer+((i - *its) * sec_jkn
                                                + (j - *jts) * sec_kn 
                                                + (k - *kts) * n
                                                + v));
                    argvlen[pos * 2 + 3] = sizeof(float);
                    pos++;
                }
            }
        }
    }

    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_check(reply);

    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
#endif
}

void redis_hmgetf_nonblock(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride,
                  int *jts, int *jte, int *jstride, int *kts, int *kte, int *kstride, int *nn, float *buffer)
{
    int i, j, k, v;
    int n = *nn;
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_k = *kte - *kts + 1;
    int sec_jkn = sec_j * sec_k * n;
    int sec_kn = sec_k * n;
    
    int i_cnt = (*ite - *its) / *istride + 1;
    int j_cnt = (*jte - *jts) / *jstride + 1;
    int k_cnt = (*kte - *kts) / *kstride + 1;
    int jkn_cnt = j_cnt * k_cnt * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = jkn_cnt;
    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));

    for(i = *its; i <= *ite; i += *istride) {
        argv[0] = "hmget";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
		offset = 0;
        for(j = *jts; j <= *jte; j += *jstride) {
            for (k = *kts; k <= *kte; k += *kstride) {
                for(v = 0; v < n; v++) {
                    sprintf(keys + offset, "%d:%d:%s", j, k, var[v]);
                    argv[pos + 2] = keys + offset;
                    argvlen[pos + 2] = strlen(keys + offset);
                    offset += argvlen[pos + 2];
                    pos++;
                }
            }
        }
 		int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }

	redis_sendall(cc);

    int mm = 0;
    for(i = *its; i <= *ite; i += *istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_returnf(reply, buffer, &mm);
        freeReplyObject(reply);
    }

    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    int kv_cnt = i_cnt * jkn_cnt;
    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));

    argv[0] = "hmget";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        for(j = *jts; j <= *jte; j += *jstride) {
            for(k = *kts; k <= *kte; k += *kstride) {
                for(v = 0; v < n; v++) {
                    sprintf(keys + offset, "%d:%d:%d:%s", i, j, k, var[v]);
                    argv[pos + 2] = keys + offset;
                    argvlen[pos + 2] = strlen(keys + offset);
                    offset += argvlen[pos + 2];
                    pos++;
                }
            }
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_returnf(reply, buffer, &mm);

    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
#endif
}

void redis_hmsetd2d_nonblock(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, int *nn, double *buffer)
{
    int i, j, v;
    int n = *nn;
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_jn = sec_j * n;
    
    int i_cnt = (*ite - *its) / *istride + 1;
    int j_cnt = (*jte - *jts) / *jstride + 1;
    int jn_cnt = j_cnt * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = jn_cnt;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));

    for(i = *its; i <= *ite; i += *istride) {
        argv[0] = "hmset";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *jts; j <= *jte; j += *jstride) {
            for(v =0; v<n; v++) {
                sprintf(keys + offset, "%d:0:%s", j, var[v]);
                argv[pos * 2 + 2] = keys + offset;
                argvlen[pos * 2 + 2] = strlen(keys + offset);
                offset += argvlen[pos * 2 + 2];
                argv[pos * 2 + 3] = (char *)(buffer+((i - *its) * sec_jn
                                            + (j - *jts) * n + v));
                argvlen[pos * 2 + 3] = sizeof(double);
                pos++;
            }
        }
        int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }

	redis_sendall(cc);

    for(i = *its; i <= *ite; i += *istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);

        freeReplyObject(reply);
    }
    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    int kv_cnt = i_cnt * jn_cnt;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));

    argv[0] = "hmset";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        for(j = *jts; j <= *jte; j += *jstride) {
            for(v = 0; v < n; v++) {
                sprintf(keys + offset, "%d:%d:0:%s", i, j, var[v]);
                argv[pos * 2 + 2] = keys + offset;
                argvlen[pos * 2 + 2] = strlen(keys + offset);
                offset += argvlen[pos * 2 + 2];
                argv[pos * 2 + 3] = (char *)(buffer+((i - *its) * sec_jn
                                            + (j - *jts) * n + v));
                argvlen[pos * 2 + 3] = sizeof(double);
                pos++;
            }
        }
    }

    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_check(reply);

    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
#endif
}

void redis_hmgetd2d_nonblock(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, int *nn, double *buffer)
{
    int i, j, v;
    int n = *nn;
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_jn = sec_j * n;
    
    int i_cnt = (*ite - *its) / *istride + 1;
    int j_cnt = (*jte - *jts) / *jstride + 1;
    int jn_cnt = j_cnt * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = jn_cnt;
    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];
  
    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        argv[0] = "hmget";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *jts; j <= *jte; j += *jstride) {
            for(v = 0; v < n; v++) {
                sprintf(keys + offset, "%d:0:%s", j, var[v]);
                argv[pos + 2] = keys + offset;
                argvlen[pos + 2] = strlen(keys + offset);
                offset += argvlen[pos + 2];
                pos++;
            }
        }
        int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }
	
	redis_sendall(cc);

    int mm = 0;
    for(i = *its; i <= *ite; i += *istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_returnd(reply, buffer, &mm);
        freeReplyObject(reply);
    }
    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    int kv_cnt = i_cnt * jn_cnt;
    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));

    argv[0] = "hmget";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        for(j = *jts; j <= *jte; j += *jstride) {
            for(v=0; v<n; v++) {
                sprintf(keys + offset, "%d:%d:0:%s", i, j, var[v]);
                argv[pos + 2] = keys + offset;
                argvlen[pos + 2] = strlen(keys + offset);
                offset += argvlen[pos + 2];
                pos++;
            }
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_returnd(reply, buffer, &mm);
    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
#endif
}

void redis_hmsetd_nonblock(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride,
                  int *jts, int *jte, int *jstride, int *kts, int *kte, int *kstride, int *nn, double *buffer)
{
    int i, j, k, v;
    int n = *nn;
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_k = *kte - *kts + 1;
    int sec_jkn = sec_j * sec_k * n;
    int sec_kn = sec_k * n;
    
    int i_cnt = (*ite - *its) / *istride + 1;
    int j_cnt = (*jte - *jts) / *jstride + 1;
    int k_cnt = (*kte - *kts) / *kstride + 1;
    int jkn_cnt = j_cnt * k_cnt * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = jkn_cnt;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        argv[0] = "hmset";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *jts; j <= *jte; j += *jstride) {
            for (k = *kts; k <= *kte; k += *kstride) {
                for(v = 0; v < n; v++) {
                    sprintf(keys + offset, "%d:%d:%s", j, k, var[v]);
                    argv[pos * 2 + 2] = keys + offset;
                    argvlen[pos * 2 + 2] = strlen(keys + offset);
                    offset += argvlen[pos * 2 + 2];
                    argv[pos * 2 + 3] = (char *)(buffer+((i - *its) * sec_jkn
                                                + (j - *jts) * sec_kn 
                                                + (k - *kts) * n
                                                + v));
                    argvlen[pos * 2 + 3] = sizeof(double);
                    pos++;
                }
            }
        }
        int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }

	redis_sendall(cc);

    for(i = *its; i <= *ite; i += *istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);

        freeReplyObject(reply);
    }

    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    int kv_cnt = i_cnt * jkn_cnt;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));

    argv[0] = "hmset";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        for(j = *jts; j <= *jte; j += *jstride) {
            for (k = *kts; k <= *kte; k += *kstride) {
                for(v = 0; v < n; v++) {
                    sprintf(keys + offset, "%d:%d:%d:%s", i, j, k, var[v]);
                    argv[pos * 2 + 2] = keys + offset;
                    argvlen[pos * 2 + 2] = strlen(keys + offset);
                    offset += argvlen[pos * 2 + 2];
                    argv[pos * 2 + 3] = (char *)(buffer+((i - *its) * sec_jkn
                                                + (j - *jts) * sec_kn 
                                                + (k - *kts) * n
                                                + v));
                    argvlen[pos * 2 + 3] = sizeof(double);
                    pos++;
                }
            }
        }
    }

    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_check(reply);

    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
#endif
}

void redis_hmgetd_nonblock(redisClusterContext *cc, char *hkey, char *varlist, int *its, int *ite, int *istride,
                  int *jts, int *jte, int *jstride, int *kts, int *kte, int *kstride, int *nn, double *buffer)
{
    int i, j, k, v;
    int n = *nn;
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_k = *kte - *kts + 1;
    int sec_jkn = sec_j * sec_k * n;
    int sec_kn = sec_k * n;
    
    int i_cnt = (*ite - *its) / *istride + 1;
    int j_cnt = (*jte - *jts) / *jstride + 1;
    int k_cnt = (*kte - *kts) / *kstride + 1;
    int jkn_cnt = j_cnt * k_cnt * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = jkn_cnt;
    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));

    for(i = *its; i <= *ite; i += *istride) {
        argv[0] = "hmget";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *jts; j <= *jte; j += *jstride) {
            for (k = *kts; k <= *kte; k += *kstride) {
                for(v = 0; v < n; v++) {
                    sprintf(keys + offset, "%d:%d:%s", j, k, var[v]);
                    argv[pos + 2] = keys + offset;
                    argvlen[pos + 2] = strlen(keys + offset);
                    offset += argvlen[pos + 2];
                    pos++;
                }
            }
        }
        int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }

	redis_sendall(cc);

    int mm = 0;
    for(i = *its; i <= *ite; i += *istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_returnd(reply, buffer, &mm);
        freeReplyObject(reply);
    }

    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    int kv_cnt = i_cnt * jkn_cnt;
    int argc = kv_cnt + 2;
    char** argv = calloc(kv_cnt + 2, sizeof(char *));
    size_t* argvlen = calloc(kv_cnt + 2, sizeof(size_t));

    argv[0] = "hmget";
    argvlen[0] = 5;
    argv[1] = hkey;
    argvlen[1] = strlen(hkey);

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *its; i <= *ite; i += *istride) {
        for(j = *jts; j <= *jte; j += *jstride) {
            for(k = *kts; k <= *kte; k += *kstride) {
                for(v = 0; v < n; v++) {
                    sprintf(keys + offset, "%d:%d:%d:%s", i, j, k, var[v]);
                    argv[pos + 2] = keys + offset;
                    argvlen[pos + 2] = strlen(keys + offset);
                    offset += argvlen[pos + 2];
                    pos++;
                }
            }
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
    reply_returnd(reply, buffer, &mm);
            
    free(argv);
    free(argvlen);
    free(keys);
    freeReplyObject(reply);
#endif
}

void redis_da_outputf_nonblock(redisClusterContext *cc, char *hkey, char *varlist, int *lonids, int *lonide, int *lonstep, 
                      int *latids, int *latide, int *latstep, int *mpas_num_lev_start, int *mpas_num_lev, int *zstep, 
                      int *num_2d, int *num_3d, float *buffer)
{
    int i, j, k, v;
    int n = (*num_2d) + (*num_3d) * (*mpas_num_lev) ;
    int sec_i = *lonide - *lonids + 1;
    int sec_j = *latide - *latids + 1;
    int sec_kjn = sec_j * n;

    int i_cnt = (*lonide - *lonids) / *lonstep + 1;
    int j_cnt = (*latide - *latids) / *latstep + 1;
    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = j_cnt * n;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char*));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *lonids; i <= *lonide; i += *lonstep) {
        argv[0] = "hmset";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *latids; j <= *latide; j += *latstep) {
            for(v = 0; v < *num_2d; v++) {
                sprintf(keys + offset, "%d:0:%s", j, var[v]);
                argv[2 * pos + 2] = keys + offset;
                argvlen[2 * pos + 2] = strlen(keys + offset);
                offset += argvlen[2 * pos + 2];
                argv[2 * pos + 3] = (char *)(buffer+((i - *lonids) * sec_kjn
                                    + (j - *latids) * n + v));
                argvlen[2 * pos + 3] = sizeof(float);
                pos++;
            }
            for(v = 0; v < *num_3d; v++) {
                for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k += *zstep) {
                    sprintf(keys + offset, "%d:%d:%s", j, k, var[*num_2d + v]);
                    argv[2 * pos + 2] = keys + offset;
                    argvlen[2 * pos + 2] = strlen(keys + offset);
                    offset += argvlen[2 * pos + 2];
                    argv[2 * pos + 3] = (char *)(buffer + ((i - *lonids) * sec_kjn
                                        +(j - *latids) * n
                                        +(*num_2d + v * (*mpas_num_lev)) + k - 1));
                    argvlen[2 * pos + 3] = sizeof(float);
                    pos++;
                }
            }
        }
        int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
    	if (ret == REDIS_ERR) {
       		printf("append command error\n");
        	exit(-1);
    	}
    }

	redis_sendall(cc);

    for(i = *lonids; i <= *lonide; i += *lonstep) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);

        freeReplyObject(reply);
    }
    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    if (n * sec_i * sec_j < 518400) {
        int kv_cnt = i_cnt * j_cnt * n;
        int argc = 2 * kv_cnt + 2;
        char** argv = calloc(argc, sizeof(char *));
        size_t* argvlen = calloc(argc, sizeof(size_t));

        argv[0] = "hmset";
        argvlen[0] = 5;
        argv[1] = hkey;
        argvlen[1] = strlen(hkey);

        int pos = 0, offset = 0;
        char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
        for(i = *lonids; i <= *lonide; i += *lonstep) {
            for(j = *latids; j <= *latide; j += *latstep) {
                for(v = 0; v < *num_2d; v++) {
                    sprintf(keys + offset, "%d:%d:0:%s", i, j, var[v]);
                    argv[2 * pos + 2] = keys + offset;
                    argvlen[2 * pos + 2] = strlen(keys + offset);
                    offset += argvlen[2 * pos + 2];
                    argv[2 * pos + 3] = (char *)(buffer+((i - *lonids) * sec_kjn
                                        + (j - *latids) * n + v));
                    argvlen[2 * pos + 3] = sizeof(float);
                    pos++;
                }
                for(v = 0; v < *num_3d; v++) {
                    for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k += *zstep) {
                        sprintf(keys + offset, "%d:%d:%d:%s", i, j, k, var[*num_2d + v]);
                        argv[2 * pos + 2] = keys + offset;
                        argvlen[2 * pos + 2] = strlen(keys + offset);
                        offset += argvlen[2 * pos + 2];
                        argv[2 * pos + 3] = (char *)(buffer+((i - *lonids) * sec_kjn
                                            +(j - *latids) * n
                                            +(*num_2d + v*(*mpas_num_lev)) + k - 1));
                        argvlen[2 * pos + 3] = sizeof(float);
                        pos++;
                    }
                }
            }
        }

        redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
        reply_check(reply);

        free(argv);
        free(argvlen);
        free(keys);
        freeReplyObject(reply);
    } else {
        redisReply *reply;
        int kv_cnt = i_cnt * n;
        int argc = 2 * kv_cnt + 2;
        char** argv = calloc(argc, sizeof(char *));
        size_t* argvlen = calloc(argc, sizeof(size_t));
        
        int pos = 0, offset = 0;
        char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
        for(j = *latids; j <= *latide; j += *latstep) {
            argv[0] = "hmset";
            argvlen[0] = 5;
            argv[1] = hkey;
            argvlen[1] = strlen(hkey);

            pos = 0;
            offset = 0;
            for(i = *lonids; i <= *lonide; i += *lonstep) {
                for(v=0; v<*num_2d; v++) {
                    sprintf(keys + offset, "%d:%d:0:%s", i, j, var[v]);
                    argv[2 * pos + 2] = keys + offset;
                    argvlen[2 * pos + 2] = strlen(keys + offset);
                    offset += argvlen[2 * pos + 2];
                    argv[2 * pos + 3] = (char *)(buffer+((i - *lonids) * sec_kjn
                                        + (j - *latids) * n + v));
                    argvlen[2 * pos + 3] = sizeof(float);
                    pos++;
                }
                for(v = 0; v < *num_3d; v++) {
                    for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                        sprintf(keys + offset, "%d:%d:%d:%s", i, j, k, var[*num_2d + v]);
                        argv[2 * pos + 2] = keys + offset;
                        argvlen[2 * pos + 2] = strlen(keys + offset);
                        offset += argvlen[2 * pos + 2];
                        argv[2 * pos + 3] = (char *)(buffer+((i - *lonids) * sec_kjn
                                            +(j - *latids) * n
                                            +(*num_2d + v*(*mpas_num_lev)) + k - 1));
                        argvlen[2 * pos + 3] = sizeof(float);
                        pos++;
                    }
                }
            }
            int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
			if (ret == REDIS_ERR) {
				printf("append command error\n");
				exit(-1);
			}
        }

		redis_sendall(cc);

        for(j = *latids; j <= *latide; j+=*latstep) {
            int r = redisClusterGetReply(cc, (void **) &reply);
            if (r == REDIS_ERR) { printf("Generic Redis Reply Error\n"); exit(-1);  }
            reply_check(reply);

            freeReplyObject(reply);
        }
        free(argv);
        free(argvlen);
        free(keys);
    }
#endif
}

void redis_da_outputd_nonblock(redisClusterContext *cc, char *hkey, char *varlist, int *lonids, int *lonide, int *lonstep, 
                      int *latids, int *latide, int *latstep, int *mpas_num_lev_start, int *mpas_num_lev, int *zstep, 
                      int *num_2d, int *num_3d, double *buffer)
{
    int i, j, k, v;
    int n = (*num_2d) + (*num_3d) * (*mpas_num_lev) ;
    int sec_i = *lonide - *lonids + 1;
    int sec_j = *latide - *latids + 1;
    int sec_kjn = sec_j * n;

    int i_cnt = (*lonide - *lonids) / *lonstep + 1;
    int j_cnt = (*latide - *latids) / *latstep + 1;
    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
    redisReply *reply;
    int kv_cnt = j_cnt * n;
    int argc = 2 * kv_cnt + 2;
    char** argv = calloc(2 * kv_cnt + 2, sizeof(char*));
    size_t* argvlen = calloc(2 * kv_cnt + 2, sizeof(size_t));
    char lon_hkey[MAX_KEY_LEN];

    int pos = 0, offset = 0;
    char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
    for(i = *lonids; i <= *lonide; i += *lonstep) {
        argv[0] = "hmset";
        argvlen[0] = 5;
        sprintf(lon_hkey, "%s:%d", hkey, i);
        argv[1] = lon_hkey;
        argvlen[1] = strlen(lon_hkey);

        pos = 0;
        offset = 0;
        for(j = *latids; j <= *latide; j += *latstep) {
            for(v = 0; v < *num_2d; v++) {
                sprintf(keys + offset, "%d:0:%s", j, var[v]);
                argv[2 * pos + 2] = keys + offset;
                argvlen[2 * pos + 2] = strlen(keys + offset);
                offset += argvlen[2 * pos + 2];
                argv[2 * pos + 3] = (char *)(buffer+((i - *lonids) * sec_kjn
                                    + (j - *latids) * n + v));
                argvlen[2 * pos + 3] = sizeof(double);
                pos++;
            }
            for(v = 0; v < *num_3d; v++) {
                for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k += *zstep) {
                    sprintf(keys + offset, "%d:%d:%s", j, k, var[*num_2d + v]);
                    argv[2 * pos + 2] = keys + offset;
                    argvlen[2 * pos + 2] = strlen(keys + offset);
                    offset += argvlen[2 * pos + 2];
                    argv[2 * pos + 3] = (char *)(buffer + ((i - *lonids) * sec_kjn
                                        + (j - *latids) * n
                                        + (*num_2d + v * (*mpas_num_lev)) + k - 1));
                    argvlen[2 * pos + 3] = sizeof(double);
                    pos++;
                }
            }
        }
		int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
		if (ret == REDIS_ERR) {
			printf("append command error\n");
			exit(-1);
		}
    }

	redis_sendall(cc);

    for(i = *lonids; i <= *lonide; i += *lonstep) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);

        freeReplyObject(reply);
    }
    free(argv);
    free(argvlen);
    free(keys);
    redisClusterReset(cc);
#else
    if (n * sec_i * sec_j < 518400) {
        int kv_cnt = i_cnt * j_cnt * n;
        int argc = 2 * kv_cnt + 2;
        char** argv = calloc(argc, sizeof(char *));
        size_t* argvlen = calloc(argc, sizeof(size_t));

        argv[0] = "hmset";
        argvlen[0] = 5;
        argv[1] = hkey;
        argvlen[1] = strlen(hkey);

        int pos = 0, offset = 0;
        char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
        for(i = *lonids; i <= *lonide; i += *lonstep) {
            for(j = *latids; j <= *latide; j += *latstep) {
                for(v = 0; v < *num_2d; v++) {
                    sprintf(keys + offset, "%d:%d:0:%s", i, j, var[v]);
                    argv[2 * pos + 2] = keys + offset;
                    argvlen[2 * pos + 2] = strlen(keys + offset);
                    offset += argvlen[2 * pos + 2];
                    argv[2 * pos + 3] = (char *)(buffer+((i - *lonids) * sec_kjn
                                        + (j - *latids) * n + v));
                    argvlen[2 * pos + 3] = sizeof(double);
                    pos++;
                }
                for(v=0; v<*num_3d; v++) {
                    for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k += *zstep) {
                        sprintf(keys + offset, "%d:%d:%d:%s", i, j, k, var[*num_2d + v]);
                        argv[2 * pos + 2] = keys + offset;
                        argvlen[2 * pos + 2] = strlen(keys + offset);
                        offset += argvlen[2 * pos + 2];
                        argv[2 * pos + 3] = (char *)(buffer+((i - *lonids) * sec_kjn
                                            +(j - *latids) * n
                                            +(*num_2d + v*(*mpas_num_lev)) + k - 1));
                        argvlen[2 * pos + 3] = sizeof(double);
                        pos++;
                    }
                }
            }
        }

        redisReply *reply = redisClusterCommandArgv(cc, argc, (const char **)argv, argvlen);
        reply_check(reply);

        free(argv);
        free(argvlen);
        free(keys);
        freeReplyObject(reply);
    } else {
        redisReply *reply;
        int kv_cnt = i_cnt * n;
        int argc = 2 * kv_cnt + 2;
        char** argv = calloc(argc, sizeof(char *));
        size_t* argvlen = calloc(argc, sizeof(size_t));
        
        int pos = 0, offset = 0;
        char* keys = calloc(kv_cnt * MAX_FIELD_LEN, sizeof(char));
        for(j = *latids; j <= *latide; j += *latstep) {
            argv[0] = "hmset";
            argvlen[0] = 5;
            argv[1] = hkey;
            argvlen[1] = strlen(hkey);

            pos = 0;
            offset = 0;
            for(i = *lonids; i <= *lonide; i += *lonstep) {
                for(v = 0; v < *num_2d; v++) {
                    sprintf(keys + offset, "%d:%d:0:%s", i, j, var[v]);
                    argv[2 * pos + 2] = keys + offset;
                    argvlen[2 * pos + 2] = strlen(keys + offset);
                    offset += argvlen[2 * pos + 2];
                    argv[2 * pos + 3] = (char *)(buffer + ((i - *lonids) * sec_kjn
                                        + (j - *latids) * n 
                                        + v));
                    argvlen[2 * pos + 3] = sizeof(double);
                    pos++;
                }
                for(v = 0; v < *num_3d; v++) {
                    for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k += *zstep) {
                        sprintf(keys + offset, "%d:%d:%d:%s", i, j, k, var[*num_2d + v]);
                        argv[2 * pos + 2] = keys + offset;
                        argvlen[2 * pos + 2] = strlen(keys + offset);
                        offset += argvlen[2 * pos + 2];
                        argv[2 * pos + 3] = (char *)(buffer + ((i - *lonids) * sec_kjn
                                            +(j - *latids) * n
                                            +(*num_2d + v*(*mpas_num_lev)) + k - 1));
                        argvlen[2 * pos + 3] = sizeof(double);
                        pos++;
                    }
                }
            }
            int ret = redisClusterAppendCommandArgv(cc, argc, (const char **)argv, argvlen);
			if (ret == REDIS_ERR) {
				printf("append command error\n");
				exit(-1);
			}
        }

		redis_sendall(cc);

        for(j = *latids; j <= *latide; j += *latstep) {
            int r = redisClusterGetReply(cc, (void **) &reply);
            if (r == REDIS_ERR) { printf("Generic Redis Reply Error\n"); exit(-1);  }
            reply_check(reply);

            freeReplyObject(reply);
        }
        free(argv);
        free(argvlen);
        free(keys);
    }
#endif
}

//----------------------------------------------------------------
// reply_interface
//----------------------------------------------------------------
void reply_returnd(redisReply *reply, double *buf, int *tag)
{
    int i;
    switch(reply->type) {
        case 1 :
            memcpy(buf, reply->str, sizeof(double));
            break;
        case 2:
            for(i = 0; i < reply->elements; i++) {
                memcpy(buf + *tag, reply->element[i]->str, sizeof(double));
                (*tag)++;
            }
            break;
        case 3:
            printf("return integer\n");
            break;
        case 4:
            printf("return null\n");
            printf("REDIS_ERR: data is null! please check your database!\n");
            exit(-1);
            break;
        case 5:
            printf("return status: %s\n", reply->str);
            break;
        case 6:
            printf("REDIS_ERR: %s!\n", reply->str);
            exit(-1);
            break;
        default:
            printf("REDIS_ERR: no match error please check redis data!\n");
            exit(-1);
    }
}

void reply_returnf(redisReply *reply, float *buf, int *tag)
{
    int i;
    switch(reply->type) {
        case 1 :
            memcpy(buf, reply->str, sizeof(float));
            break;
        case 2:
            for(i = 0; i < reply->elements; i++) {
                memcpy(buf + *tag, reply->element[i]->str, sizeof(float));
                (*tag)++;
            }
            break;
        case 3:
            printf("return integer\n");
            break;
        case 4:
            printf("return null\n");
            printf("REDIS_ERR: data is null! please check your database!\n");
            exit(-1);
            break;
        case 5:
            printf("return status: %s\n", reply->str);
            break;
        case 6:
            printf("REDIS_ERR: %s!\n", reply->str);
            exit(-1);
            break;
        default:
            printf("REDIS_ERR: no match error please check redis data!\n");
            exit(-1);
    }
}

void reply_returni(redisReply *reply, int *buf, int *tag)
{
    int i;
    switch(reply->type) {
        case 1 :
            memcpy(buf, reply->str, sizeof(int));
            break;
        case 2:
            for(i = 0; i < reply->elements; i++) {
                memcpy(buf + *tag, reply->element[i]->str, sizeof(int));
                (*tag)++;
            }
            break;
        case 3:
            printf("return integer\n");
            break;
        case 4:
            printf("return null\n");
            printf("REDIS_ERR: data is null! please check your database!\n");
            exit(-1);
            break;
        case 5:
            if(!(strcmp(reply->str,"OK")==0)) {
                printf("REDIS_ERR: return status: %s\n", reply->str);
                exit(-1);
            }
            break;
        case 6:
            printf("REDIS_ERR: %s!\n", reply->str);
            exit(-1);
            break;
        default:
            printf("REDIS_ERR: no match error please check redis data!\n");
            exit(-1);
    }
}

void reply_check(redisReply *reply)
{
    if(NULL == reply) {
        printf("REDIS_ERROR: Execut command failure\n");
        freeReplyObject(reply);
        exit(-1);
    }
    //if(!(reply->type== REDIS_REPLY_STATUS && strcmp(reply->str,"OK")==0)) {
    //    printf("failed to execute command!\n");
    //    freeReplyObject(reply);
    //    exit(-1);
    //}
}

#else
// str interface

#include "sb.h"

void RedisHgeti(redisClusterContext *cc, char *hash, char *key, int *value)
{
    redisReply *reply = redisClusterCommand(cc, "HGET %s %s", hash, key);
    reply_check(reply);
    switch(reply->type) {
        case 1 :
            *value = atoi(reply->str);
            break;
        default:
            printf("hget return no match (error)!\n");
            exit(-1);
    }
    freeReplyObject(reply);
}

void RedisHgetf(redisClusterContext *cc, char *hash, char *key, float *value)
{
    redisReply *reply = redisClusterCommand(cc, "HGET %s %s", hash, key);
    reply_check(reply);
    switch(reply->type) {
        case 1 :
            *value = atof(reply->str);
            break;
        default:
            printf("hget return no match (error)!\n");
            exit(-1);
    }
    freeReplyObject(reply);
}

void RedisHgetd(redisClusterContext *cc, char *hash, char *key, double *value)
{
    redisReply *reply = redisClusterCommand(cc, "HGET %s %s", hash, key);
    reply_check(reply);
    switch(reply->type) {
        case 1 :
            *value = atof(reply->str);
            break;
        default:
            printf("hget return no match (error)!\n");
            exit(-1);
    }
    freeReplyObject(reply);
}

char *RedisHgets(redisClusterContext *cc, char *hash, char *key, 
        char *value, int *len1, int *len2, int *len3)
{
    redisReply *reply = redisClusterCommand(cc, "HGET %s %s", hash, key);
    reply_check(reply);
    switch(reply->type) {
        case 1 :
            strcpy(value, reply->str);
            trim_string(value);
            *len3=strlen(value);
            break;
        default:
            printf("hget return no match (error)!\n");
            exit(-1);
    }
    freeReplyObject(reply);
}


void RedisHseti(redisClusterContext *cc, char *hash, char *key, int *value)
{
    redisReply *reply = redisClusterCommand(cc, "HSET %s %s %d", hash, key, *value);
    reply_check(reply);
    freeReplyObject(reply);
}

void RedisHsetf(redisClusterContext *cc, char *hash, char *key, float *value)
{
    redisReply *reply = redisClusterCommand(cc, "HSET %s %s %f", hash, key, *value);
    reply_check(reply);
    freeReplyObject(reply);
}

void RedisHsetd(redisClusterContext *cc, char *hash, char *key, double *value)
{
    redisReply *reply = redisClusterCommand(cc, "HSET %s %s %.12f", hash, key, *value);
    reply_check(reply);
    freeReplyObject(reply);
}

char *RedisHsets(redisClusterContext *cc, char *hash, char *key, char *value)
{
    redisReply *reply = redisClusterCommand(cc, "HSET %s %s %s", hash, key, value);
    reply_check(reply);
    freeReplyObject(reply);
}

void redis_hmsetf1d(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *nn, float *buffer)
{
    int i, j, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int size = (*ite - *its + 1)* n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

    sb_appendf(key, "hmset %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
          for(v=0; v<n; v++) {
              sb_appendf(key, "%d:%s %.6f ", i, var[v], *(buffer+((i - *its) * n + v)));
        }
    }

    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_check(reply);
    freeReplyObject(reply);
    sb_free(key);
}

void redis_hmsetf2d(redisClusterContext *cc, char *hkey, char *varlist, int *its, 
        int *ite, int *istride, int *jts, int *jte, int *jstride, int *nn, float *buffer)
{
    int i, j, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_jn = sec_j*n;
    int size = (*ite - *its + 1) * (*jte - *jts + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_W
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
              for(v=0; v<n; v++) {
                  sb_appendf(key, "%d:0:%s %.6f ", j, var[v], 
                          *(buffer+((i - *its) * sec_jn + (j - *jts) * n + v)));
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
              for(v=0; v<n; v++) {
                  sb_appendf(key, "%d:0:%s %.6f ", j, var[v], 
                          *(buffer+((i - *its) * sec_jn + (j - *jts) * n + v)));
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_check(reply);
        freeReplyObject(reply);
    }
#endif
#else
    sb_appendf(key, "hmset %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(j = *jts; j <= *jte; j+=*jstride) {
              for(v=0; v<n; v++) {
                  sb_appendf(key, "%d:%d:0:%s %.6f ", i, j, var[v], 
                          *(buffer+((i - *its) * sec_jn + (j - *jts) * n + v)));
            }
        }
    }

    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_check(reply);
    freeReplyObject(reply);
#endif
    sb_free(key);
}

void redis_hmsetf(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, 
        int *kms, int *kme, int *kstride, int *nn, float *buffer)
{
    int i, j, k, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_k = *kme - *kms + 1;
    int sec_kjn = sec_k * sec_j*n;
    int sec_kn = sec_k*n;
    int size = (*ite - *its + 1) * (*jte - *jts + 1) * (*kme - *kms + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_W
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%s %.6f ", j, k, var[v], 
                            *(buffer+((i - *its) * sec_kjn
                            +(j - *jts) * sec_kn
                            +(k - *kms)*n + v)));
                }
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    for(i = *its; i <= *ite; i+=*istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%s %.6f ", j, k, var[v], 
                            *(buffer+((i - *its) * sec_kjn
                            +(j - *jts) * sec_kn
                            +(k - *kms)*n + v)));
                }
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_check(reply);
        freeReplyObject(reply);
    }
#endif
#else
    sb_appendf(key, "hmset %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%d:%s %.6f ", i, j, k, var[v], 
                            *(buffer+((i - *its) * sec_kjn
                            +(j - *jts) * sec_kn
                            +(k - *kms)*n + v)));
                }
            }
        }
    }

    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_check(reply);
    freeReplyObject(reply);
#endif
    sb_free(key);
}

void redis_hmgetf1d(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *nn, float *buffer)
{
    int i, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int size = (*ite - *its + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

    sb_appendf(key, "hmget %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(v=0; v<n; v++) {
            sb_appendf(key, "%d:%s ", i, var[v]); 
        }
    }

    int mm=0;
    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_returnf(reply, buffer, &mm);
    freeReplyObject(reply);
    sb_free(key);
}

void redis_hmgetf2d(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, 
        int *nn, float *buffer)
{
    int i, j, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_jn = sec_j*n;
    int size = (*ite - *its + 1) * (*jte - *jts + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_R
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmget %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(v=0; v<n; v++) {
                sb_appendf(key, "%d:0:%s ", j, var[v]); 
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    int mm = 0;
    for(i = *its; i <= *ite; i+=*istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_returnf(reply, buffer, &mm);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    int mm = 0;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmget %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(v=0; v<n; v++) {
                sb_appendf(key, "%d:0:%s ", j, var[v]); 
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_returnf(reply, buffer, &mm);
        freeReplyObject(reply);
    }
#endif
#else
    sb_appendf(key, "hmget %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(v=0; v<n; v++) {
                sb_appendf(key, "%d:%d:0:%s ", i, j, var[v]); 
            }
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_returnf(reply, buffer, &mm);
    freeReplyObject(reply);
#endif
    sb_free(key);
}

void redis_hmgetf(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, 
        int *kms, int *kme, int *kstride, int *nn, float *buffer)
{
    int i, j, k, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_k = *kme - *kms + 1;
    int sec_kjn = sec_k * sec_j*n;
    int sec_kn = sec_k*n;
    int size = (*ite - *its + 1) * (*jte - *jts + 1) * (*kme - *kms + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_R
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmget %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%s ", j, k, var[v]); 
                }
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    int mm = 0;
    for(i = *its; i <= *ite; i+=*istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_returnf(reply, buffer, &mm);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    int mm = 0;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmget %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%s ", j, k, var[v]); 
                }
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_returnf(reply, buffer, &mm);
        freeReplyObject(reply);
    }
#endif
#else
    sb_appendf(key, "hmget %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%d:%s ", i, j, k, var[v]); 
                }
            }
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_returnf(reply, buffer, &mm);
    freeReplyObject(reply);
#endif
    sb_free(key);
}

void redis_hmsetd1d(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *nn, double *buffer)
{
    int i, j, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int size = (*ite - *its + 1)* n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

    sb_appendf(key, "hmset %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
          for(v=0; v<n; v++) {
              sb_appendf(key, "%d:%s %.12f ", i, var[v], *(buffer+((i - *its) * n + v)));
        }
    }

    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    freeReplyObject(reply);
    sb_free(key);
}

void redis_hmsetd2d(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, 
        int *nn, double *buffer)
{
    int i, j, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_jn = sec_j*n;
    int size = (*ite - *its + 1) * (*jte - *jts + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_W
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
              for(v=0; v<n; v++) {
                  sb_appendf(key, "%d:0:%s %.12f ", j, var[v], 
                          *(buffer+((i - *its) * sec_jn
                          +(j - *jts) * n + v)));
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
              for(v=0; v<n; v++) {
                  sb_appendf(key, "%d:0:%s %.12f ", j, var[v], 
                          *(buffer+((i - *its) * sec_jn
                          +(j - *jts) * n + v)));
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_check(reply);
        freeReplyObject(reply);
    }
#endif
#else
    sb_appendf(key, "hmset %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(j = *jts; j <= *jte; j+=*jstride) {
              for(v=0; v<n; v++) {
                  sb_appendf(key, "%d:%d:0:%s %.12f ", i, j, var[v], 
                          *(buffer+((i - *its) * sec_jn
                          +(j - *jts) * n + v)));
            }
        }
    }

    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_check(reply);
    freeReplyObject(reply);
#endif
    sb_free(key);
}

void redis_hmsetd(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, 
        int *kms, int *kme, int *kstride, int *nn, double *buffer)
{
    int i, j, k, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_k = *kme - *kms + 1;
    int sec_kjn = sec_k * sec_j*n;
    int sec_kn = sec_k*n;
    int size = (*ite - *its + 1) * (*jte - *jts + 1) * (*kme - *kms + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_W
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%s %.12f ", j, k, var[v], 
                            *(buffer+((i - *its) * sec_kjn
                            +(j - *jts) * sec_kn
                            +(k - *kms)*n + v)));
                }
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%s %.12f ", j, k, var[v], 
                            *(buffer+((i - *its) * sec_kjn
                            +(j - *jts) * sec_kn
                            +(k - *kms)*n + v)));
                }
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_check(reply);
        freeReplyObject(reply);
    }
#endif
#else
    sb_appendf(key, "hmset %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%d:%s %.12f ", i, j, k, var[v], 
                            *(buffer+((i - *its) * sec_kjn
                            +(j - *jts) * sec_kn
                            +(k - *kms)*n + v)));
                }
            }
        }
    }

    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_check(reply);
    freeReplyObject(reply);
#endif
    sb_free(key);
}

void redis_hmgetd1d(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *nn, double *buffer)
{
    int i, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int size = (*ite - *its + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

    sb_appendf(key, "hmget %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(v=0; v<n; v++) {
            sb_appendf(key, "%d:%s ", i, var[v]); 
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_returnd(reply, buffer, &mm);
    freeReplyObject(reply);
    sb_free(key);
}

void redis_hmgetd2d(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, 
        int *nn, double *buffer)
{
    int i, j, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_jn = sec_j*n;
    int size = (*ite - *its + 1) * (*jte - *jts + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_R
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmget %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(v=0; v<n; v++) {
                sb_appendf(key, "%d:0:%s ", j, var[v]); 
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    int mm = 0;
    for(i = *its; i <= *ite; i+=*istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_returnd(reply, buffer, &mm);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    int mm = 0;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmget %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(v=0; v<n; v++) {
                sb_appendf(key, "%d:0:%s ", j, var[v]); 
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_returnd(reply, buffer, &mm);
        freeReplyObject(reply);
    }
#endif
#else
    sb_appendf(key, "hmget %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(v=0; v<n; v++) {
                sb_appendf(key, "%d:%d:0:%s ", i, j, var[v]); 
            }
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_returnd(reply, buffer, &mm);
    freeReplyObject(reply);
#endif
    sb_free(key);
}

void redis_hmgetd(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, 
        int *kms, int *kme, int *kstride, int *nn, double *buffer)
{
    int i, j, k, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_k = *kme - *kms + 1;
    int sec_kjn = sec_k * sec_j*n;
    int sec_kn = sec_k*n;
    int size = (*ite - *its + 1) * (*jte - *jts + 1) * (*kme - *kms + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_R
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmget %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%s ", j, k, var[v]); 
                }
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    int mm = 0;
    for(i = *its; i <= *ite; i+=*istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_returnd(reply, buffer, &mm);
        freeReplyObject(reply);
    }
    //redisClusterReset(cc);
#else
    int mm = 0;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmget %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%s ", j, k, var[v]); 
                }
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_returnd(reply, buffer, &mm);
        freeReplyObject(reply);
    }
#endif
#else
    sb_appendf(key, "hmget %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%d:%s ", i, j, k, var[v]); 
                }
            }
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_returnd(reply, buffer, &mm);
    freeReplyObject(reply);
#endif
    sb_free(key);
}

void redis_da_outputd(redisClusterContext *cc, char *hkey, char *varlist, 
        int *lonids, int *lonide, int *lonstep, 
        int *latids, int *latide, int *latstep, int *mpas_num_lev_start, 
        int *mpas_num_lev, int *zstep, int *num_2d, int *num_3d, double *buffer)
{
    int i, j, k, v;
    int n = (*num_2d) + (*num_3d) * (*mpas_num_lev) ;
	StringBuilder	*key = sb_create();
    int sec_i = *lonide - *lonids + 1;
    int sec_j = *latide - *latids + 1;
    int sec_kjn = sec_j*n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }
#ifdef LON_SLICE
#ifdef REDIS_PIPE_W
    redisReply *reply;
    for(i = *lonids; i <= *lonide; i+=*lonstep) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *latids; j <= *latide; j+=*latstep) {
            for(v=0; v<*num_2d; v++) {
                sb_appendf(key, "%d:0:%s %.12f ", j, var[v], 
                        *(buffer+((i - *lonids) * sec_kjn
                        +(j - *latids) * n + v)));
            }
            for(v=0; v<*num_3d; v++) {
                for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                    sb_appendf(key, "%d:%d:%s %.12f ", j, k, var[*num_2d+v], 
                            *(buffer+((i - *lonids) * sec_kjn
                            +(j - *latids) * n
                            +(*num_2d + v*(*mpas_num_lev)) + k - 1)));
                }
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    for(i = *lonids; i <= *lonide; i+=*lonstep) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    for(i = *lonids; i <= *lonide; i+=*lonstep) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *latids; j <= *latide; j+=*latstep) {
            for(v=0; v<*num_2d; v++) {
                sb_appendf(key, "%d:0:%s %.12f ", j, var[v], 
                        *(buffer+((i - *lonids) * sec_kjn
                        +(j - *latids) * n + v)));
            }
            for(v=0; v<*num_3d; v++) {
                for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                    sb_appendf(key, "%d:%d:%s %.12f ", j, k, var[*num_2d+v], 
                            *(buffer+((i - *lonids) * sec_kjn
                            +(j - *latids) * n
                            +(*num_2d + v*(*mpas_num_lev)) + k - 1)));
                }
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_check(reply);
        freeReplyObject(reply);
    }
#endif
#else
    if (n * sec_i * sec_j < 518400) {
        sb_appendf(key, "hmset %s ", hkey);
        for(i = *lonids; i <= *lonide; i+=*lonstep) {
            for(j = *latids; j <= *latide; j+=*latstep) {
                for(v=0; v<*num_2d; v++) {
                    sb_appendf(key, "%d:%d:0:%s %.12f ", i, j, var[v], 
                            *(buffer+((i - *lonids) * sec_kjn
                            +(j - *latids) * n + v)));
                }
                for(v=0; v<*num_3d; v++) {
                    for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                        sb_appendf(key, "%d:%d:%d:%s %.12f ", i, j, k, 
                                var[*num_2d+v], *(buffer+((i - *lonids) * sec_kjn
                                +(j - *latids) * n
                                +(*num_2d + v*(*mpas_num_lev)) + k - 1)));
                    }
                }
            }
        }

        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_check(reply);
        freeReplyObject(reply);
    } else {
        redisReply *reply;
        for(j = *latids; j <= *latide; j+=*latstep) {
            sb_reset(key);
            sb_appendf(key, "hmset %s ", hkey);
            for(i = *lonids; i <= *lonide; i+=*lonstep) {
                for(v=0; v<*num_2d; v++) {
                    sb_appendf(key, "%d:%d:0:%s %.12f ", i, j, var[v], 
                            *(buffer+((i - *lonids) * sec_kjn
                            +(j - *latids) * n + v)));
                }
                for(v=0; v<*num_3d; v++) {
                    for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                        sb_appendf(key, "%d:%d:%d:%s %.12f ", i, j, k, var[*num_2d+v], 
                                *(buffer+((i - *lonids) * sec_kjn
                                +(j - *latids) * n
                                +(*num_2d + v*(*mpas_num_lev)) + k - 1)));
                    }
                }
            }
            redisClusterAppendCommand(cc, sb_concat(key));
        }

        for(j = *latids; j <= *latide; j+=*latstep) {
            int r = redisClusterGetReply(cc, (void **) &reply);
            if (r == REDIS_ERR) { printf("Generic Redis Reply Error\n"); exit(-1);  }
            freeReplyObject(reply);
        }
    }
#endif
    sb_free(key);
}

void redis_da_outputf(redisClusterContext *cc, char *hkey, char *varlist, 
        int *lonids, int *lonide, int *lonstep, 
        int *latids, int *latide, int *latstep, int *mpas_num_lev_start, 
        int *mpas_num_lev, int *zstep, int *num_2d, int *num_3d, float *buffer)
{
    int i, j, k, v;
    int n = (*num_2d) + (*num_3d) * (*mpas_num_lev) ;
	StringBuilder	*key = sb_create();
    int sec_i = *lonide - *lonids + 1;
    int sec_j = *latide - *latids + 1;
    int sec_kjn = sec_j*n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_W
    redisReply *reply;
    for(i = *lonids; i <= *lonide; i+=*lonstep) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *latids; j <= *latide; j+=*latstep) {
            for(v=0; v<*num_2d; v++) {
                sb_appendf(key, "%d:0:%s %.6f ", j, var[v], 
                        *(buffer+((i - *lonids) * sec_kjn
                        +(j - *latids) * n + v)));
            }
            for(v=0; v<*num_3d; v++) {
                for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                    sb_appendf(key, "%d:%d:%s %.6f ", j, k, var[*num_2d+v], 
                            *(buffer+((i - *lonids) * sec_kjn
                            +(j - *latids) * n
                            +(*num_2d + v*(*mpas_num_lev)) + k - 1)));
                }
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    for(i = *lonids; i <= *lonide; i+=*lonstep) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    for(i = *lonids; i <= *lonide; i+=*lonstep) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *latids; j <= *latide; j+=*latstep) {
            for(v=0; v<*num_2d; v++) {
                sb_appendf(key, "%d:0:%s %.6f ", j, var[v], 
                        *(buffer+((i - *lonids) * sec_kjn
                        +(j - *latids) * n + v)));
            }
            for(v=0; v<*num_3d; v++) {
                for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                    sb_appendf(key, "%d:%d:%s %.6f ", j, k, var[*num_2d+v], 
                            *(buffer+((i - *lonids) * sec_kjn
                            +(j - *latids) * n
                            +(*num_2d + v*(*mpas_num_lev)) + k - 1)));
                }
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_check(reply);
        freeReplyObject(reply);
    }
#endif
#else
    if (n * sec_i * sec_j < 518400) {
        sb_appendf(key, "hmset %s ", hkey);
        for(i = *lonids; i <= *lonide; i+=*lonstep) {
            for(j = *latids; j <= *latide; j+=*latstep) {
                for(v=0; v<*num_2d; v++) {
                    sb_appendf(key, "%d:%d:0:%s %.6f ", i, j, var[v], 
                            *(buffer+((i - *lonids) * sec_kjn
                            +(j - *latids) * n + v)));
                }
                for(v=0; v<*num_3d; v++) {
                    for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                        sb_appendf(key, "%d:%d:%d:%s %.6f ", i, j, k, var[*num_2d+v], 
                                *(buffer+((i - *lonids) * sec_kjn
                                +(j - *latids) * n
                                +(*num_2d + v*(*mpas_num_lev)) + k - 1)));
                    }
                }
            }
        }

        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        freeReplyObject(reply);
    } else {
        redisReply *reply;
        for(j = *latids; j <= *latide; j+=*latstep) {
            sb_reset(key);
            sb_appendf(key, "hmset %s ", hkey);
            for(i = *lonids; i <= *lonide; i+=*lonstep) {
                for(v=0; v<*num_2d; v++) {
                    sb_appendf(key, "%d:%d:0:%s %.6f ", i, j, var[v], 
                            *(buffer+((i - *lonids) * sec_kjn
                            +(j - *latids) * n + v)));
                }
                for(v=0; v<*num_3d; v++) {
                    for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                        sb_appendf(key, "%d:%d:%d:%s %.6f ", i, j, k, var[*num_2d+v], 
                                *(buffer+((i - *lonids) * sec_kjn
                                +(j - *latids) * n
                                +(*num_2d + v*(*mpas_num_lev)) + k - 1)));
                    }
                }
            }
            redisClusterAppendCommand(cc, sb_concat(key));
        }

        for(j = *latids; j <= *latide; j+=*latstep) {
            int r = redisClusterGetReply(cc, (void **) &reply);
            if (r == REDIS_ERR) { printf("Generic Redis Reply Error\n"); exit(-1);  }
            freeReplyObject(reply);
        }
    }
#endif
    sb_free(key);
}



//-------------------------------------------------
// non-block interfaces
//-------------------------------------------------

void redis_hmsetf2d_nonblock(redisClusterContext *cc, char *hkey, char *varlist, int *its, 
        int *ite, int *istride, int *jts, int *jte, int *jstride, int *nn, float *buffer)
{
    int i, j, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_jn = sec_j*n;
    int size = (*ite - *its + 1) * (*jte - *jts + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_W
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
              for(v=0; v<n; v++) {
                  sb_appendf(key, "%d:0:%s %.6f ", j, var[v], 
                          *(buffer+((i - *its) * sec_jn + (j - *jts) * n + v)));
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    redis_sendall(cc);

    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
              for(v=0; v<n; v++) {
                  sb_appendf(key, "%d:0:%s %.6f ", j, var[v], 
                          *(buffer+((i - *its) * sec_jn + (j - *jts) * n + v)));
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_check(reply);
        freeReplyObject(reply);
    }
#endif
#else
    sb_appendf(key, "hmset %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(j = *jts; j <= *jte; j+=*jstride) {
              for(v=0; v<n; v++) {
                  sb_appendf(key, "%d:%d:0:%s %.6f ", i, j, var[v], 
                          *(buffer+((i - *its) * sec_jn + (j - *jts) * n + v)));
            }
        }
    }

    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_check(reply);
    freeReplyObject(reply);
#endif
    sb_free(key);
}

void redis_hmsetf_nonblock(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, 
        int *kms, int *kme, int *kstride, int *nn, float *buffer)
{
    int i, j, k, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_k = *kme - *kms + 1;
    int sec_kjn = sec_k * sec_j*n;
    int sec_kn = sec_k*n;
    int size = (*ite - *its + 1) * (*jte - *jts + 1) * (*kme - *kms + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_W
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%s %.6f ", j, k, var[v], 
                            *(buffer+((i - *its) * sec_kjn
                            +(j - *jts) * sec_kn
                            +(k - *kms)*n + v)));
                }
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    redis_sendall(cc);

    for(i = *its; i <= *ite; i+=*istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%s %.6f ", j, k, var[v], 
                            *(buffer+((i - *its) * sec_kjn
                            +(j - *jts) * sec_kn
                            +(k - *kms)*n + v)));
                }
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_check(reply);
        freeReplyObject(reply);
    }
#endif
#else
    sb_appendf(key, "hmset %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%d:%s %.6f ", i, j, k, var[v], 
                            *(buffer+((i - *its) * sec_kjn
                            +(j - *jts) * sec_kn
                            +(k - *kms)*n + v)));
                }
            }
        }
    }

    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_check(reply);
    freeReplyObject(reply);
#endif
    sb_free(key);
}

void redis_hmgetf2d_nonblock(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, 
        int *nn, float *buffer)
{
    int i, j, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_jn = sec_j*n;
    int size = (*ite - *its + 1) * (*jte - *jts + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_R
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmget %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(v=0; v<n; v++) {
                sb_appendf(key, "%d:0:%s ", j, var[v]); 
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    redis_sendall(cc);

    int mm = 0;
    for(i = *its; i <= *ite; i+=*istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_returnf(reply, buffer, &mm);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    int mm = 0;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmget %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(v=0; v<n; v++) {
                sb_appendf(key, "%d:0:%s ", j, var[v]); 
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_returnf(reply, buffer, &mm);
        freeReplyObject(reply);
    }
#endif
#else
    sb_appendf(key, "hmget %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(v=0; v<n; v++) {
                sb_appendf(key, "%d:%d:0:%s ", i, j, var[v]); 
            }
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_returnf(reply, buffer, &mm);
    freeReplyObject(reply);
#endif
    sb_free(key);
}

void redis_hmgetf_nonblock(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, 
        int *kms, int *kme, int *kstride, int *nn, float *buffer)
{
    int i, j, k, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_k = *kme - *kms + 1;
    int sec_kjn = sec_k * sec_j*n;
    int sec_kn = sec_k*n;
    int size = (*ite - *its + 1) * (*jte - *jts + 1) * (*kme - *kms + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_R
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmget %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%s ", j, k, var[v]); 
                }
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    redis_sendall(cc);

    int mm = 0;
    for(i = *its; i <= *ite; i+=*istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
<<<<<<< HEAD
        reply_returnf(reply, buffer, &mm);
=======
>>>>>>> 7d19bd245a087b334695121392ec1189dafe8fed
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    int mm = 0;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmget %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%s ", j, k, var[v]); 
                }
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_returnf(reply, buffer, &mm);
        freeReplyObject(reply);
    }
#endif
#else
    sb_appendf(key, "hmget %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%d:%s ", i, j, k, var[v]); 
                }
            }
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_returnf(reply, buffer, &mm);
    freeReplyObject(reply);
#endif
    sb_free(key);
}

void redis_hmsetd2d_nonblock(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, 
        int *nn, double *buffer)
{
    int i, j, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_jn = sec_j*n;
    int size = (*ite - *its + 1) * (*jte - *jts + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_W
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
              for(v=0; v<n; v++) {
                  sb_appendf(key, "%d:0:%s %.12f ", j, var[v], 
                          *(buffer+((i - *its) * sec_jn
                          +(j - *jts) * n + v)));
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }
    
    redis_sendall(cc);

    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
              for(v=0; v<n; v++) {
                  sb_appendf(key, "%d:0:%s %.12f ", j, var[v], 
                          *(buffer+((i - *its) * sec_jn
                          +(j - *jts) * n + v)));
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_check(reply);
        freeReplyObject(reply);
    }
#endif
#else
    sb_appendf(key, "hmset %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(j = *jts; j <= *jte; j+=*jstride) {
              for(v=0; v<n; v++) {
                  sb_appendf(key, "%d:%d:0:%s %.12f ", i, j, var[v], 
                          *(buffer+((i - *its) * sec_jn
                          +(j - *jts) * n + v)));
            }
        }
    }

    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_check(reply);
    freeReplyObject(reply);
#endif
    sb_free(key);
}

void redis_hmsetd_nonblock(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, 
        int *kms, int *kme, int *kstride, int *nn, double *buffer)
{
    int i, j, k, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_k = *kme - *kms + 1;
    int sec_kjn = sec_k * sec_j*n;
    int sec_kn = sec_k*n;
    int size = (*ite - *its + 1) * (*jte - *jts + 1) * (*kme - *kms + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_W
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%s %.12f ", j, k, var[v], 
                            *(buffer+((i - *its) * sec_kjn
                            +(j - *jts) * sec_kn
                            +(k - *kms)*n + v)));
                }
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    redis_sendall(cc);

    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%s %.12f ", j, k, var[v], 
                            *(buffer+((i - *its) * sec_kjn
                            +(j - *jts) * sec_kn
                            +(k - *kms)*n + v)));
                }
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_check(reply);
        freeReplyObject(reply);
    }
#endif
#else
    sb_appendf(key, "hmset %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%d:%s %.12f ", i, j, k, var[v], 
                            *(buffer+((i - *its) * sec_kjn
                            +(j - *jts) * sec_kn
                            +(k - *kms)*n + v)));
                }
            }
        }
    }

    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_check(reply);
    freeReplyObject(reply);
#endif
    sb_free(key);
}

void redis_hmgetd2d_nonblock(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, 
        int *nn, double *buffer)
{
    int i, j, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_jn = sec_j*n;
    int size = (*ite - *its + 1) * (*jte - *jts + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_R
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmget %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(v=0; v<n; v++) {
                sb_appendf(key, "%d:0:%s ", j, var[v]); 
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    redis_sendall(cc);

    int mm = 0;
    for(i = *its; i <= *ite; i+=*istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_returnd(reply, buffer, &mm);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    int mm = 0;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmget %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(v=0; v<n; v++) {
                sb_appendf(key, "%d:0:%s ", j, var[v]); 
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_returnd(reply, buffer, &mm);
        freeReplyObject(reply);
    }
#endif
#else
    sb_appendf(key, "hmget %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(v=0; v<n; v++) {
                sb_appendf(key, "%d:%d:0:%s ", i, j, var[v]); 
            }
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_returnd(reply, buffer, &mm);
    freeReplyObject(reply);
#endif
    sb_free(key);
}

void redis_hmgetd_nonblock(redisClusterContext *cc, char *hkey, char *varlist, 
        int *its, int *ite, int *istride, int *jts, int *jte, int *jstride, 
        int *kms, int *kme, int *kstride, int *nn, double *buffer)
{
    int i, j, k, v;
    int n = *nn;
	StringBuilder	*key = sb_create();
    int sec_i = *ite - *its + 1;
    int sec_j = *jte - *jts + 1;
    int sec_k = *kme - *kms + 1;
    int sec_kjn = sec_k * sec_j*n;
    int sec_kn = sec_k*n;
    int size = (*ite - *its + 1) * (*jte - *jts + 1) * (*kme - *kms + 1) * n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_R
    redisReply *reply;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmget %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%s ", j, k, var[v]); 
                }
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    redis_sendall(cc);

    int mm = 0;
    for(i = *its; i <= *ite; i+=*istride) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_returnd(reply, buffer, &mm);
        freeReplyObject(reply);
    }
    //redisClusterReset(cc);
#else
    int mm = 0;
    for(i = *its; i <= *ite; i+=*istride) {
        sb_reset(key);
        sb_appendf(key, "hmget %s:%d ", hkey, i);
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%s ", j, k, var[v]); 
                }
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_returnd(reply, buffer, &mm);
        freeReplyObject(reply);
    }
#endif
#else
    sb_appendf(key, "hmget %s ", hkey);
    for(i = *its; i <= *ite; i+=*istride) {
        for(j = *jts; j <= *jte; j+=*jstride) {
            for(k = *kms; k <= *kme; k+=*kstride) {
                for(v=0; v<n; v++) {
                    sb_appendf(key, "%d:%d:%d:%s ", i, j, k, var[v]); 
                }
            }
        }
    }

    int mm = 0;
    redisReply *reply = redisClusterCommand(cc, sb_concat(key));
    reply_returnd(reply, buffer, &mm);
    freeReplyObject(reply);
#endif
    sb_free(key);
}

void redis_da_outputd_nonblock(redisClusterContext *cc, char *hkey, char *varlist, 
        int *lonids, int *lonide, int *lonstep, 
        int *latids, int *latide, int *latstep, int *mpas_num_lev_start, 
        int *mpas_num_lev, int *zstep, int *num_2d, int *num_3d, double *buffer)
{
    int i, j, k, v;
    int n = (*num_2d) + (*num_3d) * (*mpas_num_lev) ;
	StringBuilder	*key = sb_create();
    int sec_i = *lonide - *lonids + 1;
    int sec_j = *latide - *latids + 1;
    int sec_kjn = sec_j*n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }
#ifdef LON_SLICE
#ifdef REDIS_PIPE_W
    redisReply *reply;
    for(i = *lonids; i <= *lonide; i+=*lonstep) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *latids; j <= *latide; j+=*latstep) {
            for(v=0; v<*num_2d; v++) {
                sb_appendf(key, "%d:0:%s %.12f ", j, var[v], 
                        *(buffer+((i - *lonids) * sec_kjn
                        +(j - *latids) * n + v)));
            }
            for(v=0; v<*num_3d; v++) {
                for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                    sb_appendf(key, "%d:%d:%s %.12f ", j, k, var[*num_2d+v], 
                            *(buffer+((i - *lonids) * sec_kjn
                            +(j - *latids) * n
                            +(*num_2d + v*(*mpas_num_lev)) + k - 1)));
                }
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    redis_sendall(cc);

    for(i = *lonids; i <= *lonide; i+=*lonstep) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    for(i = *lonids; i <= *lonide; i+=*lonstep) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *latids; j <= *latide; j+=*latstep) {
            for(v=0; v<*num_2d; v++) {
                sb_appendf(key, "%d:0:%s %.12f ", j, var[v], 
                        *(buffer+((i - *lonids) * sec_kjn
                        +(j - *latids) * n + v)));
            }
            for(v=0; v<*num_3d; v++) {
                for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                    sb_appendf(key, "%d:%d:%s %.12f ", j, k, var[*num_2d+v], 
                            *(buffer+((i - *lonids) * sec_kjn
                            +(j - *latids) * n
                            +(*num_2d + v*(*mpas_num_lev)) + k - 1)));
                }
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_check(reply);
        freeReplyObject(reply);
    }
#endif
#else
    if (n * sec_i * sec_j < 518400) {
        sb_appendf(key, "hmset %s ", hkey);
        for(i = *lonids; i <= *lonide; i+=*lonstep) {
            for(j = *latids; j <= *latide; j+=*latstep) {
                for(v=0; v<*num_2d; v++) {
                    sb_appendf(key, "%d:%d:0:%s %.12f ", i, j, var[v], 
                            *(buffer+((i - *lonids) * sec_kjn
                            +(j - *latids) * n + v)));
                }
                for(v=0; v<*num_3d; v++) {
                    for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                        sb_appendf(key, "%d:%d:%d:%s %.12f ", i, j, k, 
                                var[*num_2d+v], *(buffer+((i - *lonids) * sec_kjn
                                +(j - *latids) * n
                                +(*num_2d + v*(*mpas_num_lev)) + k - 1)));
                    }
                }
            }
        }

        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_check(reply);
        freeReplyObject(reply);
    } else {
        redisReply *reply;
        for(j = *latids; j <= *latide; j+=*latstep) {
            sb_reset(key);
            sb_appendf(key, "hmset %s ", hkey);
            for(i = *lonids; i <= *lonide; i+=*lonstep) {
                for(v=0; v<*num_2d; v++) {
                    sb_appendf(key, "%d:%d:0:%s %.12f ", i, j, var[v], 
                            *(buffer+((i - *lonids) * sec_kjn
                            +(j - *latids) * n + v)));
                }
                for(v=0; v<*num_3d; v++) {
                    for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                        sb_appendf(key, "%d:%d:%d:%s %.12f ", i, j, k, var[*num_2d+v], 
                                *(buffer+((i - *lonids) * sec_kjn
                                +(j - *latids) * n
                                +(*num_2d + v*(*mpas_num_lev)) + k - 1)));
                    }
                }
            }
            redisClusterAppendCommand(cc, sb_concat(key));
        }

	redis_sendall(cc);

        for(j = *latids; j <= *latide; j+=*latstep) {
            int r = redisClusterGetReply(cc, (void **) &reply);
            if (r == REDIS_ERR) { printf("Generic Redis Reply Error\n"); exit(-1);  }
            freeReplyObject(reply);
        }
    }
#endif
    sb_free(key);
}

void redis_da_outputf_nonblock(redisClusterContext *cc, char *hkey, char *varlist, 
        int *lonids, int *lonide, int *lonstep, 
        int *latids, int *latide, int *latstep, int *mpas_num_lev_start, 
        int *mpas_num_lev, int *zstep, int *num_2d, int *num_3d, float *buffer)
{
    int i, j, k, v;
    int n = (*num_2d) + (*num_3d) * (*mpas_num_lev) ;
	StringBuilder	*key = sb_create();
    int sec_i = *lonide - *lonids + 1;
    int sec_j = *latide - *latids + 1;
    int sec_kjn = sec_j*n;

    /*string split*/
    char var[n][32];
    char* token = strtok( varlist, ":");
    i = 0;
    while( token != NULL )
    {
        strcpy(var[i], token); 
        token = strtok( NULL, ":");
        i++;
    }

#ifdef LON_SLICE
#ifdef REDIS_PIPE_W
    redisReply *reply;
    for(i = *lonids; i <= *lonide; i+=*lonstep) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *latids; j <= *latide; j+=*latstep) {
            for(v=0; v<*num_2d; v++) {
                sb_appendf(key, "%d:0:%s %.6f ", j, var[v], 
                        *(buffer+((i - *lonids) * sec_kjn
                        +(j - *latids) * n + v)));
            }
            for(v=0; v<*num_3d; v++) {
                for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                    sb_appendf(key, "%d:%d:%s %.6f ", j, k, var[*num_2d+v], 
                            *(buffer+((i - *lonids) * sec_kjn
                            +(j - *latids) * n
                            +(*num_2d + v*(*mpas_num_lev)) + k - 1)));
                }
            }
        }
        redisClusterAppendCommand(cc, sb_concat(key));
    }

    redis_sendall(cc);

    for(i = *lonids; i <= *lonide; i+=*lonstep) {
        int r = redisClusterGetReply(cc, (void **)&reply);
        if(r == REDIS_ERR) {
            printf("Redis Reply Error!\n");
            exit(-1);
        }
        reply_check(reply);
        freeReplyObject(reply);
    }
    redisClusterReset(cc);
#else
    for(i = *lonids; i <= *lonide; i+=*lonstep) {
        sb_reset(key);
        sb_appendf(key, "hmset %s:%d ", hkey, i);
        for(j = *latids; j <= *latide; j+=*latstep) {
            for(v=0; v<*num_2d; v++) {
                sb_appendf(key, "%d:0:%s %.6f ", j, var[v], 
                        *(buffer+((i - *lonids) * sec_kjn
                        +(j - *latids) * n + v)));
            }
            for(v=0; v<*num_3d; v++) {
                for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                    sb_appendf(key, "%d:%d:%s %.6f ", j, k, var[*num_2d+v], 
                            *(buffer+((i - *lonids) * sec_kjn
                            +(j - *latids) * n
                            +(*num_2d + v*(*mpas_num_lev)) + k - 1)));
                }
            }
        }
        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        reply_check(reply);
        freeReplyObject(reply);
    }
#endif
#else
    if (n * sec_i * sec_j < 518400) {
        sb_appendf(key, "hmset %s ", hkey);
        for(i = *lonids; i <= *lonide; i+=*lonstep) {
            for(j = *latids; j <= *latide; j+=*latstep) {
                for(v=0; v<*num_2d; v++) {
                    sb_appendf(key, "%d:%d:0:%s %.6f ", i, j, var[v], 
                            *(buffer+((i - *lonids) * sec_kjn
                            +(j - *latids) * n + v)));
                }
                for(v=0; v<*num_3d; v++) {
                    for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                        sb_appendf(key, "%d:%d:%d:%s %.6f ", i, j, k, var[*num_2d+v], 
                                *(buffer+((i - *lonids) * sec_kjn
                                +(j - *latids) * n
                                +(*num_2d + v*(*mpas_num_lev)) + k - 1)));
                    }
                }
            }
        }

        redisReply *reply = redisClusterCommand(cc, sb_concat(key));
        freeReplyObject(reply);
    } else {
        redisReply *reply;
        for(j = *latids; j <= *latide; j+=*latstep) {
            sb_reset(key);
            sb_appendf(key, "hmset %s ", hkey);
            for(i = *lonids; i <= *lonide; i+=*lonstep) {
                for(v=0; v<*num_2d; v++) {
                    sb_appendf(key, "%d:%d:0:%s %.6f ", i, j, var[v], 
                            *(buffer+((i - *lonids) * sec_kjn
                            +(j - *latids) * n + v)));
                }
                for(v=0; v<*num_3d; v++) {
                    for(k = *mpas_num_lev_start; k <= *mpas_num_lev; k+=*zstep) {
                        sb_appendf(key, "%d:%d:%d:%s %.6f ", i, j, k, var[*num_2d+v], 
                                *(buffer+((i - *lonids) * sec_kjn
                                +(j - *latids) * n
                                +(*num_2d + v*(*mpas_num_lev)) + k - 1)));
                    }
                }
            }
            redisClusterAppendCommand(cc, sb_concat(key));
        }

	redis_sendall(cc);

        for(j = *latids; j <= *latide; j+=*latstep) {
            int r = redisClusterGetReply(cc, (void **) &reply);
            if (r == REDIS_ERR) { printf("Generic Redis Reply Error\n"); exit(-1);  }
            freeReplyObject(reply);
        }
    }
#endif
    sb_free(key);
}

//-----------------------------------------------------------
// reply
//-----------------------------------------------------------
void reply_returnd(redisReply *reply, double *buf, int *tag)
{
    int i;
    switch(reply->type) {
        case 1 :
            *buf = atof(reply->str);
            break;
        case 2:
            for(i = 0; i < reply->elements; i++) {
                *(buf + *tag) = atof(reply->element[i]->str);
                (*tag)++;
            }
            break;
        case 3:
            printf("return integer\n");
            break;
        case 4:
            printf("return null\n");
            printf("REDIS_ERR: data is null! please check your database!\n");
            exit(-1);
            break;
        case 5:
            printf("return status: %s\n", reply->str);
            break;
        case 6:
            printf("REDIS_ERR: %s!\n", reply->str);
            exit(-1);
            break;
        default:
            printf("REDIS_ERR: no match error please check redis data!\n");
            exit(-1);
    }
}

void reply_returnf(redisReply *reply, float *buf, int *tag)
{
    int i;
    switch(reply->type) {
        case 1 :
            *buf = atof(reply->str);
            break;
        case 2:
            for(i = 0; i < reply->elements; i++) {
                *(buf + *tag) = atof(reply->element[i]->str);
                (*tag)++;
            }
            break;
        case 3:
            printf("return integer\n");
            break;
        case 4:
            printf("return null\n");
            printf("REDIS_ERR: data is null! please check your database!\n");
            exit(-1);
            break;
        case 5:
            printf("return status: %s\n", reply->str);
            break;
        case 6:
            printf("REDIS_ERR: %s!\n", reply->str);
            exit(-1);
            break;
        default:
            printf("REDIS_ERR: no match error please check redis data!\n");
            exit(-1);
    }
}

void reply_returni(redisReply *reply, int *buf, int *tag)
{
    int i;
    switch(reply->type) {
        case 1 :
            *buf = atoi(reply->str);
            break;
        case 2:
            for(i = 0; i < reply->elements; i++) {
                *(buf + *tag) = atoi(reply->element[i]->str);
                (*tag)++;
            }
            break;
        case 3:
            printf("return integer\n");
            break;
        case 4:
            printf("return null\n");
            printf("REDIS_ERR: data is null! please check your database!\n");
            exit(-1);
            break;
        case 5:
            if(!(strcmp(reply->str,"OK")==0)) {
                printf("REDIS_ERR: return status: %s\n", reply->str);
                exit(-1);
            }
            break;
        case 6:
            printf("REDIS_ERR: %s!\n", reply->str);
            exit(-1);
            break;
        default:
            printf("REDIS_ERR: no match error please check redis data!\n");
            exit(-1);
    }
}

void reply_check(redisReply *reply)
{
    if(NULL == reply) {
        printf("REDIS_ERROR: Execut command failure\n");
        freeReplyObject(reply);
        exit(-1);
    }
}

#endif
