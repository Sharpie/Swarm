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
  int stack_offset;  /* Can be negative! */
}

#define MFRAME_ARGS struct hppa_args

#define MFRAME_INIT_ARGS(CUM, RTYPE) \
({ (CUM).reg_offset = 0; \
   (CUM).stack_offset = 0; \
} 

#define MFRAME_ARG_ENCODING(CUM, TYPE, STACK, DEST) \
({ \
  const char* type = (TYPE); \
  size_t align = objc_alignof_type (type); \
  size_t size = objc_sizeof_type (type); \
  unsigned typelen; \
  BOOL register_flag; \
  \
  (TYPE) = objc_skip_typespec (type); \
  typelen = (TYPE) - type; \
  register_flag = *(TYPE) == '+' && *type != _C_DBL && *type != _C_FLT; \
  if (register_flag) \ 
    { \
      (TYPE)++; \
      (CUM).reg_offset = atoi (TYPE); \
      sprintf ((DEST), "%.*s+%d", typelen, type, (CUM).reg_offset); \
    } \
  else \
    { \
      if (*(TYPE) == '+' || *(TYPE) == '-') \
        (TYPE)++; \
      (CUM).stack_offset = atoi (TYPE); \
      sprintf ((DEST), "%.*s%d", typelen, type, (CUM).stack_offset); \
      (STACK) += size; \
    } \
  while (isdigit ((int) *(TYPE))) \
    { \
      (TYPE)++; \
    } \
  (DEST) = &(DEST)[strlen(DEST)]; \
  if (register_flag) \
   (CUM).reg_offset += size; \
  else \
   (CUM).stack_offset += size; \
})

