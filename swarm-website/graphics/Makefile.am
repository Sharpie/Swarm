## graphics

heatbugsPNG=$(addprefix heatbugs-, controls.png screen.png screenhalf.png unhappy.png world.png worldhalf.png worldthumb.png)

mousetrapPNG=$(addprefix mousetrap-, graphs.png world1.png world3.png world2.png world4.png)

EPS = sdg.eps sdg-small-source.eps swarmfest2001.eps
PNG = $(patsubst %.eps, %.png, $(EPS)) 
JPEG = $(patsubst %.png, %.jpeg, $(PNG))	
JPEG += $(patsubst %.png, %.gif, $(PNG))	
graphicsdir = $(prefix)/graphics
graphics_DATA = sdg-small.png sdg.jpeg $(heatbugsPNG) $(mousetrapPNG)

%.png: %.eps
	convert -quality 100% $^ $@

%.jpeg: %.png
	convert -crop 0x0 -geometry 50% -quality 100% $^ $@

sdg-small.png: sdg-small-source.png
	convert -geometry 55% -quality 100% $^ $@

## icons

iconsPNG = iconsmallrightarrow.png iconat.png warning.png note.png tip.png important.png caution.png home.png next.png prev.png toc-blank.png toc-minus.png toc-plus.png rarrow.png

iconsdir = $(prefix)/graphics/icons
icons_DATA = $(iconsPNG) 

GENERATED_GRAPHICS = $(PNG) $(JPEG)
CLEANFILES = $(GENERATED_GRAPHICS)
EXTRA_DIST = $(EPS) $(iconsPNG) $(heatbugsPNG) $(mousetrapPNG)
