include $(top_builddir)/etc/Makefile.tests

MODULE=tactivity

check_PROGRAMS = ForEachRandomized ForEach ActionGroupRandomized ActionGroup ConcurrentGroupRandomized ConcurrentGroup Schedule DynamicSchedule DynamicScheduleRepeat EmptySchedule 

type_SRCS = ActionGroup_test.m ActionGroup_test.h ConcurrentGroup_test.h ConcurrentGroup_test.m tactivity.h tactivity.m

include $(top_srcdir)/tests/Makefile.rules

EXTRA_DIST = Holder.h Holder.m

$(OBJECTS): $(TYPEINFO)

Responder_SRCS = Responder.m Responder.h
ForEachRandomized_SOURCES = ForEachRandomized.m $(Responder_SRCS) $(type_SRCS)

ForEach_SOURCES = ForEach.m $(Responder_SRCS) $(type_SRCS)

ActionGroupRandomized_SOURCES = ActionGroupRandomized.m $(Responder_SRCS) $(type_SRCS)

ActionGroup_SOURCES = ActionGroup.m $(Responder_SRCS) $(type_SRCS)

ConcurrentGroupRandomized_SOURCES = ConcurrentGroupRandomized.m $(Responder_SRCS) $(type_SRCS)

ConcurrentGroup_SOURCES = ConcurrentGroup.m $(Responder_SRCS) $(type_SRCS)

Schedule_SOURCES = Schedule.m $(Responder_SRCS)
DynamicSchedule_SOURCES = DynamicSchedule.m DSSwarm.m DSSwarm.h 
DynamicScheduleRepeat_SOURCES = DynamicScheduleRepeat.m DSRSwarm.m DSRSwarm.h 
EmptySchedule_SOURCES = EmptySchedule.m ESSwarm.m ESSwarm.h

TESTS = $(check_PROGRAMS)
TESTS_ENVIRONMENT = SWARM_INHIBIT_ARCHIVER_LOAD=1

