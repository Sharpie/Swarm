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

struct hppa_args {
  unsigned reg_offset;
  int stack_offset;
  unsigned count;
};

#define MFRAME_ARGS struct hppa_args

#define MFRAME_INIT_ARGS(CUM, RTYPE) \
({ \
 (CUM).reg_offset = 20; \
 (CUM).stack_offset = -20; \
 (CUM).count = 0; \
})

#define MFRAME_ARG_ENCODING(CUM, TYPE, STACK, DEST) \
({  \
  const char* type = (TYPE); \
  BOOL double_flag = *type == _C_DBL; \
  unsigned offset; \
  BOOL register_flag; \
  \
  (TYPE) = objc_skip_typespec (type); \
  \
  register_flag = ((CUM).count < (double_flag ? 3 : 4)); \
  offset = register_flag ? (CUM).reg_offset : (CUM).stack_offset; \
  if (double_flag && (offset & 7)) \
    offset -= 4; \
  sprintf ((DEST), "%.*s%s%d", ((TYPE) - type), type, \
    register_flag ? "+" : "", offset); \
  if (*(TYPE) == '+') \
    (TYPE)++; \
  while (isdigit ((int) *(TYPE))) \
    { \
      (TYPE)++; \
    } \
  (DEST)=&(DEST)[strlen (DEST)]; \
  if (double_flag) \
    (CUM).count++; \
  if (register_flag) \
    (CUM).reg_offset = offset - 4; \
  else \
    (CUM).stack_offset = offset - 4; \
  (CUM).count++; \
})

