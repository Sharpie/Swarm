/* See ../README for copyright */

#define	MFRAME_STACK_STRUCT	1
#define	MFRAME_STRUCT_BYREF	1
#define MFRAME_SMALL_STRUCT	8
#define MFRAME_ARGS_SIZE	56
#define MFRAME_RESULT_SIZE	16

#define MFRAME_GET_STRUCT_ADDR(ARGS, TYPES) \
((*(TYPES)==_C_STRUCT_B || *(TYPES)==_C_UNION_B || *(TYPES)==_C_ARY_B) ? \
      **(void***)(ARGS) : (void*)0)

#define MFRAME_SET_STRUCT_ADDR(ARGS, TYPES, ADDR) \
({if (*(TYPES)==_C_STRUCT_B || *(TYPES)==_C_UNION_B || *(TYPES)==_C_ARY_B) \
      **(void***)(ARGS) = (ADDR);})

#define MFRAME_ARGS int

#define MFRAME_INIT_ARGS(CUM, RTYPE) (CUM) = 0 \

#define MFRAME_ARG_ENCODING(CUM, TYPE, STACK, DEST) \
({ \
  const char* type = (TYPE); \
  size_t size = objc_sizeof_type (type); \
  int typelen; \
  int offset; \
  \
  (TYPE) = objc_skip_typespec (type); \
  typelen = (TYPE) - type; \
  offset = atoi (TYPE); \
  if (*(TYPE) == '+' && !(*type == _C_DBL || *type == _C_FLT)) \
    { \
      sprintf ((DEST), "%.*s+%d", typelen, type, offset); \
      (TYPE)++; \
      (CUM) += size; \
    } \
  else \
    { \
      if (offset <= 40 && offset >= 0) \
        offset += 12; \
      sprintf ((DEST), "%.*s%d", typelen, type, offset); \
      (STACK) += size; \
      if (*(TYPE) == '+' || *(TYPE) == '-') \
        (TYPE)++; \
    } \
  while (isdigit ((int) *(TYPE))) \
    { \
      (TYPE)++; \
    } \
  (DEST) = &(DEST)[strlen(DEST)]; \
})

