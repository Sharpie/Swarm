#define MFRAME_STACK_STRUCT 1
#define MFRAME_SMALL_STRUCT 16
#define MFRAME_STRUCT_BYREF 0
#define MFRAME_ARGS_SIZE 136
#define MFRAME_RESULT_SIZE 16 

inline static BOOL
refstructp (const char *types)
{
  return ((*types == _C_STRUCT_B || *types ==_C_UNION_B)
          && objc_sizeof_type (types) > MFRAME_SMALL_STRUCT);
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
  unsigned reg_offset;
  unsigned float_reg_offset;
  unsigned stack_offset;
};
     
#define MFRAME_ARGS struct mips_args

#define MFRAME_INIT_ARGS(CUM, RTYPE) \
({ \
 (CUM).reg_offset = 0; \
 (CUM).float_reg_offset = 0; \
 (CUM).stack_offset = 96; \
})

#define MFRAME_ARG_ENCODING(CUM, TYPE, STACK, DEST) \
({  \
  const char* type = (TYPE); \
  unsigned align = objc_alignof_type (type); \
  unsigned size = objc_sizeof_type (type); \
  BOOL float_flag = *type == _C_FLT; \
  BOOL double_flag = *type == _C_DBL; \
  BOOL structref_flag = refstructp (type); \
  unsigned offset; \
  BOOL register_flag = NO; \
  \
  (TYPE) = objc_skip_typespec (type); \
  \
  if (*(TYPE) == '+') \
    register_flag = YES; \
  if (!structref_flag) \
    { \
      if (register_flag) \
        { \
          offset = atoi (TYPE); \
          if (float_flag || double_flag) \
            { \
              if (float_flag) \
                offset += 4; \
              offset = ROUND (offset, align); \
              (CUM).float_reg_offset = offset; \
            } \
          else \
            { \
              offset += 4; \
              offset = ROUND (offset, align); \
              (CUM).reg_offset = offset; \
            } \
        } \
      else \
        { \
          offset = (CUM).stack_offset; \
          offset = ROUND (offset, align); \
        } \
    } \
  else \
    { \
      offset = (CUM).reg_offset; \
      register_flag = YES; \
      offset = ROUND (offset, 8); \
    } \
  sprintf ((DEST), "%.*s%s%d", ((TYPE) - type), type, \
	   register_flag ? "+" : "", offset); \
  if (*(TYPE) == '+') \
    { \
      (TYPE)++; \
    } \
  while (isdigit ((int) *(TYPE))) \
    { \
      (TYPE)++; \
    } \
  (DEST)=&(DEST)[strlen (DEST)]; \
  if (register_flag) \
    { \
      (CUM).float_reg_offset += size; \
      (CUM).reg_offset += size; \
    } \
  else \
    (CUM).stack_offset += size; \
})

