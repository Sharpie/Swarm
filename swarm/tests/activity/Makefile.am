include $(top_builddir)/etc/Makefile.tests

MODULE=tactivity

check_PROGRAMS = ForEachRandomized ForEach ActionGroupRandomized ActionGroup ConcurrentGroupRandomized ConcurrentGroup Schedule DynamicSchedule DynamicScheduleRepeat EmptySchedule 

type_SOURCES = ActionGroup_test.m ActionGroup_test.h ConcurrentGroup_test.h ConcurrentGroup_test.m tactivity.h tactivity.m

include $(top_srcdir)/tests/Makefile.rules

EXTRA_DIST = Holder.h Holder.m

check: headers

Responder_SOURCES = Responder.m Responder.h
ForEachRandomized_SOURCES = ForEachRandomized.m $(Responder_SOURCES) $(type_SOURCES)
ForEach_SOURCES = ForEach.m $(Responder_SOURCES) $(type_SOURCES)
ActionGroupRandomized_SOURCES = ActionGroupRandomized.m $(Responder_SOURCES) $(type_SOURCES)
ActionGroup_SOURCES = ActionGroup.m $(Responder_SOURCES) $(type_SOURCES)
ConcurrentGroupRandomized_SOURCES = ConcurrentGroupRandomized.m $(Responder_SOURCES) $(type_SOURCES)
ConcurrentGroup_SOURCES = ConcurrentGroup.m $(Responder_SOURCES) $(type_SOURCES)
Schedule_SOURCES = Schedule.m $(Responder_SOURCES)
DynamicSchedule_SOURCES = DynamicSchedule.m DSSwarm.m DSSwarm.h 
DynamicScheduleRepeat_SOURCES = DynamicScheduleRepeat.m DSRSwarm.m DSRSwarm.h 
EmptySchedule_SOURCES = EmptySchedule.m ESSwarm.m ESSwarm.h

TESTS = $(check_PROGRAMS)
TESTS_ENVIRONMENT = SWARM_INHIBIT_ARCHIVER_LOAD=1

