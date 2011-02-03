.SUFFIXES: .png
.NOTPARALLEL:

all: examples

EXAMPLES = example1.png example2.png example3.png example4.png example5.png \
     example6_draft.png example6_aa.png example6.png example6_alpha.png \
     example7.png example8.png example9.png

DIFFS = $(EXAMPLES:%.png=%_diff.png)

compare: $(DIFFS)

$(DIFFS): %_diff.png: %.png reference/%.png
	compare -metric PSNR $^ $@

save_reference: $(EXAMPLES)
	mkdir -p reference
	cp -f $(EXAMPLES) reference/

# If it doesn't exist, just use the normal version
../render_small:
	ln -s render ../render_small

examples: $(EXAMPLES)

example1.png: ../render_small
	R3D_LIB=../materials ../render_small -gamma 1.1 -png example1.png < example1.r3d 

example2.png: ../render_small ../balls ../rods
	cat header2.r3d plane.r3d > example2.r3d
	cat chaincolours.pdb LT.pdb | ../balls -h >> example2.r3d
	cat sugarcolours.pdb sugars.pdb | ../rods  -h >> example2.r3d
	../render_small -png example2.png < example2.r3d
	rm -f example2.r3d

example3.png: ../render_small
	../render_small -png example3.png < example3.r3d

example4.png: ../render_small
	molscript -r < example3.mol | ../render_small -png example4.png

example5.png: ../render_small
	../render_small -png example5.png < example5.r3d

example6_draft.png: ../render_small
	R3D_LIB=../materials ../render_small -size 345x345 -draft -png example6_draft.png < example6.r3d

example6_aa.png: ../render_small
	R3D_LIB=../materials ../render_small -size 345x345 -aa -png example6_aa.png < example6.r3d

example6.png: ../render_small
	R3D_LIB=../materials ../render_small -size 345x345 -png example6.png < example6.r3d

example6_alpha.png: ../render_small
	R3D_LIB=../materials ../render_small -size 345x345 -alpha -png example6_alpha.png < example6.r3d

example7.png: ../stereo3d ../render
	PATH=../:$$PATH ../stereo3d -png < example7.r3d | convert -resize 50% png:- example7.png
        
example8.png: ../balls ../ribbon ../stereo3d ../render
	cat header8.r3d > example8.r3d
	grep ' [DEFGH] ' LT.pdb | cat chaincolours.pdb - | ../balls -h >> example8.r3d
	grep ' C ' LT.pdb | ../ribbon -h -d1 >> example8.r3d
	PATH=../:$$PATH ../stereo3d -png < example8.r3d | convert -gamma 1.2 png:- example8.png
	rm -f example8.r3d

example9.png: ../rods ../rastep
	rm -rf setup.angles setup.matrix temp.tmp
	../rods < sugarG.pdb | render | convert -resize 50%x50% png:- example9a.png
	../rastep < sugarG.pdb | render | convert -resize 50%x50% png:- example9b.png
	../rastep -fancy2 -prob 30 < sugarG.pdb | render | convert -resize 50%x50% png:- example9c.png
	../rastep -fancy3 < sugarG.pdb | render | convert -resize 50%x50% png:- example9d.png
	../rastep -Bcolor 6. 26. < sugarG.pdb | render | convert -resize 50%x50% png:- example9e.png
	../rastep -fancy6 -Bcolor 6. 26. < sugarG.pdb | render | convert -resize 50%x50% png:- example9f.png
	montage -bordercolor black -borderwidth 1 -geometry 100%x100%+0+0 \
	    -label "GM1-OS" example9a.png \
	    -label "50% probability" example9b.png \
	    -label "fancy2 30% probability" example9c.png \
	    -label "fancy3" example9d.png \
	    -label "colored by B value" example9e.png \
	    -label "fancy6 for ORTEP lovers" example9f.png \
	    example9_labels.png
	montage -gamma 1.0 -geometry 100%x100%+0+0 \
	    example9a.png example9b.png example9c.png \
	    example9d.png example9e.png example9f.png \
	    example9.png
	#rm -f example9[abcdef].png

clean:
	rm -f $(EXAMPLES) $(DIFFS)
	rm -f example2.r3d example8.r3d example9[abcdef].png example9_labels.png
	rm -rf reference

