/* See ../README for copyright */

/*
 * First six arguments are passed in registers with small (< sizeof(void*))
 * values occupying the space of a pointer.
 * If the method returns a structure, it's address is passed as an invisible
 * first argument.
 */

#define MFRAME_STACK_STRUCT 1
#define MFRAME_STRUCT_BYREF     0
#define MFRAME_SMALL_STRUCT     0
#define MFRAME_ARGS_SIZE        104
#define MFRAME_RESULT_SIZE      16
#define MFRAME_FLT_IN_FRAME_AS_DBL      0

/*
 * Structures are passed by reference as an invisible first argument, so
 * they go in the first space on the stack.
 */
#define MFRAME_GET_STRUCT_ADDR(ARGS, TYPES) \
((*(TYPES)==_C_STRUCT_B || *(TYPES)==_C_UNION_B || *(TYPES)==_C_ARY_B) ? \
      ((void**)(ARGS))[1] : (void*)0)

#define MFRAME_SET_STRUCT_ADDR(ARGS, TYPES, ADDR) \
({if (*(TYPES)==_C_STRUCT_B || *(TYPES)==_C_UNION_B || *(TYPES)==_C_ARY_B) \
      ((void**)(ARGS))[1] = (ADDR);})


#if 0
/*
 * Declare a type for keeping track of the arguments processed.
 */
typedef struct alpha_args {
  int   reg_pos;
  int   stk_pos;
} MFRAME_ARGS;


/*
 * Initialize a variable to keep track of argument info while processing a
 * method.  Keeps count of the offset of arguments on the stack.
 * This offset is adjusted to take account of an invisible first argument
 * used to return structures.
 */

#define MFRAME_INIT_ARGS(CUM, RTYPE) \
({ \
  (CUM).reg_pos = (*(RTYPE)==_C_STRUCT_B || *(RTYPE)==_C_UNION_B || \
      *(RTYPE)==_C_ARY_B) ? 16 : 8; \
  (CUM).stk_pos = 0; \
})

/*
 * Define maximum register offset - after this, stuff goes on the stack.
 */
#define ALPHAMAXR       56

#define MFRAME_ARG_ENCODING(CUM, TYPE, STACK, DEST) \
({  \
  const char* type = (TYPE); \
  int align, size; \
\
  (TYPE) = objc_skip_typespec(type); \
  align = objc_alignof_type (type); \
  size = objc_sizeof_type (type); \
  size = ROUND(size, sizeof(void*)); \
\
  if ((CUM).reg_pos + size > ALPHAMAXR) (CUM).reg_pos = ALPHAMAXR; \
  if ((CUM).reg_pos == ALPHAMAXR) \
    { \
      sprintf((DEST), "%.*s%d", (int) ((TYPE)-type), type, (CUM).stk_pos); \
      (CUM).stk_pos += size; \
      (STACK) = (CUM).stk_pos; \
    } \
  else \
    { \
      sprintf((DEST), "%.*s+%d", (int) ((TYPE)-type), type, (CUM).reg_pos); \
      (CUM).reg_pos += size; \
    } \
  (DEST)=&(DEST)[strlen(DEST)]; \
  if (*(TYPE) == '+') \
    { \
      (TYPE)++; \
    } \
  while (isDigit (*(TYPE))) \
    { \
      (TYPE)++; \
    } \
})
#else

struct alpha_args {
  unsigned reg_offset;
  unsigned float_reg_offset;
  unsigned stack_offset;
  unsigned count;
};

#define MFRAME_ARGS struct alpha_args

#define MFRAME_INIT_ARGS(CUM, RTYPE) \
({ \
 (CUM).reg_offset = 8; \
 (CUM).float_reg_offset = 56; \
 (CUM).stack_offset = 96; \
 (CUM).count = 0; \
})

#define MFRAME_ARG_ENCODING(CUM, TYPE, STACK, DEST) \
({  \
  const char* type = (TYPE); \
  const char* outtype = type; \
  unsigned offset; \
  BOOL double_flag = *type == _C_DBL; \
  BOOL float_flag = *type == _C_FLT; \
  BOOL register_flag; \
  \
  if (float_flag) \
    outtype = @encode (double); \
  else \
    outtype = type; \
  (TYPE) = objc_skip_typespec (type); \
  \
  register_flag = ((CUM).count < 6); \
  offset = register_flag ? ((float_flag || double_flag) \
                             ? (CUM).float_reg_offset \
                             : (CUM).reg_offset) \
                         : (CUM).stack_offset; \
  sprintf ((DEST), "%.*s%s%u", (int) ((TYPE) - type), outtype, \
    register_flag ? "+" : "", offset); \
  if (*(TYPE) == '+') \
    (TYPE)++; \
  while (isDigit (*(TYPE))) \
    { \
      (TYPE)++; \
    } \
  (DEST) = &(DEST)[strlen (DEST)]; \
  if (register_flag) \
    { \
      (CUM).float_reg_offset += 8; \
      (CUM).reg_offset += 8; \
    } \
  else \
    (CUM).stack_offset += 8; \
  (CUM).count++; \
})


#endif
