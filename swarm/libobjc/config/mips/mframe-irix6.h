#define MFRAME_STACK_STRUCT 1
#define MFRAME_SMALL_STRUCT 16
#define MFRAME_STRUCT_BYREF 0
#define MFRAME_ARGS_SIZE 136
#define MFRAME_RESULT_SIZE 16 
#define MFRAME_FLT_IN_FRAME_AS_DBL 1

inline static BOOL
refstructp (const char *types)
{
  return ((*types == _C_STRUCT_B || *types ==_C_UNION_B)
          && objc_sizeof_type (types) > MFRAME_SMALL_STRUCT);
}

inline static BOOL
floatp (const char *types)
{
  return *types == _C_FLT || *types == _C_DBL;
}

inline static void **
mframe_get_struct_addr_ptr (arglist_t args, const char *types)
{
  return refstructp (types)
    ? *(void **) args + 1
    : NULL;
}
    
#define MFRAME_GET_STRUCT_ADDR(ARGS, TYPES) \
({ void **ptr = mframe_get_struct_addr_ptr (ARGS, TYPES); \
   ptr ? *ptr : NULL; })

#define MFRAME_SET_STRUCT_ADDR(ARGS, TYPES, ADDR) \
( { void **ptr = mframe_get_struct_addr_ptr (ARGS, TYPES); \
    if (ptr) *ptr = (ADDR); } )

struct mips_args {
  int reg_offset;
  int float_reg_offset;
};
     
#define MFRAME_ARGS struct mips_args

#define MFRAME_INIT_ARGS(CUM, RTYPE) \
({ \
 (CUM).reg_offset = 20; \
 (CUM).float_reg_offset = 88; \
})

#define MFRAME_ARG_ENCODING(CUM, TYPE, STACK, DEST) \
({  \
  const char* type = (TYPE); \
  unsigned align = objc_alignof_type (type); \
  unsigned size = objc_sizeof_type (type); \
  BOOL structref_flag = refstructp (type); \
  BOOL float_flag = floatp (type); \
  unsigned offset = structref_flag ? 4 \
                    : float_flag ? (CUM).float_reg_offset : (CUM).reg_offset; \
  \
  offset = ROUND (offset, align); \
  (TYPE) = objc_skip_typespec (type); \
  \
  if (*(TYPE) == '+') \
    { \
      (TYPE)++; \
    } \
  sprintf ((DEST), "%.*s%d", (int) ((TYPE)-type), type, offset); \
  while (isdigit ((int) *(TYPE))) \
    { \
      (TYPE)++; \
    } \
  (DEST)=&(DEST)[strlen (DEST)]; \
  if (!structref_flag) \
    { \
      if (float_flag) \
        (CUM).float_reg_offset = offset + size; \
      else \
        (CUM).reg_offset = offset + size; \
    } \
})

