SWARMHOME=../..
LIBNAME=space
OBJECTS=Discrete2d.o DblBuffer2d.o Ca2d.o Diffuse2d.o ConwayLife2d.o Value2dDisplay.o Object2dDisplay.o Grid2d.o
HEADERS=Discrete2d.h DblBuffer2d.h Ca2d.h Diffuse2d.h ConwayLife2d.h Value2dDisplay.h Object2dDisplay.h Grid2d.h 
include $(SWARMHOME)/Makefile.lib

Discrete2d.o: Discrete2d.h Discrete2d.m
DblBuffer2d.o: DblBuffer2d.m DblBuffer2d.h Discrete2d.h
Ca2d.o: Ca2d.m Ca2d.h DblBuffer2d.h
ConwayLife2d.o: ConwayLife2d.m ConwayLife2d.h Ca2d.h
Diffuse2d.o: Diffuse2d.m Diffuse2d.h Ca2d.h
Value2dDisplay.o: Value2dDisplay.m Value2dDisplay.h Ca2d.h
Grid2d.o: Grid2d.m Grid2d.h Discrete2d.h
Object2dDisplay.o: Object2dDisplay.m Object2dDisplay.h
