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
  unsigned reg_count;
};
     
#define MFRAME_ARGS struct mips_args

#define MFRAME_INIT_ARGS(CUM, RTYPE) \
({ \
 (CUM).reg_offset = 12; \
 (CUM).float_reg_offset = 72; \
 (CUM).stack_offset = 20; \
 (CUM).reg_count = 0; \
})

#define MFRAME_ARG_ENCODING(CUM, TYPE, STACK, DEST) \
({  \
  const char* type = (TYPE); \
  BOOL float_flag = *type == _C_FLT; \
  BOOL double_flag = *type == _C_DBL; \
  BOOL structref_flag = refstructp (type); \
  unsigned offset; \
  BOOL register_flag; \
  \
  (TYPE) = objc_skip_typespec (type); \
  \
  if ((CUM).reg_count < 4 || float_flag || double_flag) \
    register_flag = YES; \
  else \
    register_flag = NO; \
  if (!structref_flag) \
    { \
      if (register_flag) \
        { \
          if (float_flag || double_flag) \
            { \
              offset = (CUM).float_reg_offset; \
              if (float_flag) \
                offset += 4; \
            } \
          else \
            offset = (CUM).reg_offset; \
        } \
      else \
        offset = (CUM).stack_offset; \
    } \
  else \
    { \
      offset = (CUM).reg_offset; \
      register_flag = YES; \
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
      if (!(float_flag || double_flag)) \
        (CUM).reg_offset += 8; \
      (CUM).float_reg_offset += 8; \
      (CUM).reg_count++; \
    } \
  if ((CUM).reg_count > 4 || !register_flag) \
    (CUM).stack_offset += 8; \
})

