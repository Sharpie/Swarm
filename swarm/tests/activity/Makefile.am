include $(top_builddir)/etc/Makefile.tests

check_PROGRAMS = ForEachRandomized ForEach ActionGroupRandomized ActionGroup ConcurrentGroupRandomized ConcurrentGroup Schedule DynamicSchedule DynamicScheduleRepeat EmptySchedule 

EXTRA_DIST = Holder.h Holder.m

Responder_SOURCES = Responder.m Responder.h
ActionGroup_test_SOURCES = ActionGroup_test.m ActionGroup_test.h
ConcurrentGroup_test_SOURCES = ConcurrentGroup_test.m ConcurrentGroup_test.h
ForEachRandomized_SOURCES = ForEachRandomized.m $(Responder_SOURCES) $(ActionGroup_test_SOURCES)
ForEach_SOURCES = ForEach.m $(Responder_SOURCES) $(ActionGroup_test_SOURCES)
ActionGroupRandomized_SOURCES = ActionGroupRandomized.m $(Responder_SOURCES) $(ActionGroup_test_SOURCES)
ActionGroup_SOURCES = ActionGroup.m $(Responder_SOURCES) $(ActionGroup_test_SOURCES)
ConcurrentGroupRandomized_SOURCES = ConcurrentGroupRandomized.m $(Responder_SOURCES) $(ConcurrentGroup_test_SOURCES)
ConcurrentGroup_SOURCES = ConcurrentGroup.m $(Responder_SOURCES) $(ConcurrentGroup_test_SOURCES)
Schedule_SOURCES = Schedule.m $(Responder_SOURCES)
DynamicSchedule_SOURCES = DynamicSchedule.m DSSwarm.m DSSwarm.h 
DynamicScheduleRepeat_SOURCES = DynamicScheduleRepeat.m DSRSwarm.m DSRSwarm.h 
EmptySchedule_SOURCES = EmptySchedule.m ESSwarm.m ESSwarm.h

TESTS = $(check_PROGRAMS)
TESTS_ENVIRONMENT = SWARM_INHIBIT_ARCHIVER_LOAD=1
