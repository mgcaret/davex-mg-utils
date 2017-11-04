DAVEX=../davex-code/src
ACMD=java -jar ~/bin/AppleCommander-1.3.5-ac.jar
NULIB2=nulib2
BOOTDSK=~/vii_hd.2mg
CA65=ca65
LD65=utils/auto_origin.sh ld65
GENHELP=utils/gen_help.sh
MG_CMDS=at.info.p8c at.zones.p8c afp.userprefix.p8c afp.sessions.p8c alias.p8c at.boot.p8c deschw.p8c dmem.p8c nbp.lookup.p8c tardis.p8c nbp.parse.p8c iie.card.p8c idemu.p8c mig.insp.p8c fastchip.p8c afp.timezone.p8c setyear.p8c

.PHONY: all
all: shk ;

.phony: shk
shk: DAVEX.MG.SHK ;

.phony: disk
disk: davex.mg.po ;

davex.mg.po: $(MG_CMDS)
	$(ACMD) -pro140 davex.mg.po DAVEX.MG
	set -x; for CMD in $(MG_CMDS); do $(ACMD) -p davex.mg.po $${CMD%.*} 'BIN' '0x8001' < $$CMD; done
	#set -x; for HLP in help/*; do $(ACMD) -p davex.mg.po $$HLP 'TXT' < $$HLP; done

# Adjust for your emulation scenario
emulate: davex.mg.po
	open $(BOOTDSK) davex.mg.po -a 'Virtual ]['

DAVEX.MG.SHK: $(MG_CMDS)
	rm -f DAVEX.MG.SHK
	set -x; for CMD in $(MG_CMDS); do cp -p $$CMD "$${CMD%.*}#2E8001"; done
	$(NULIB2) -ae DAVEX.MG.SHK *2E8001
	rm -f *2E8001
	set -x; for HLP in help/*; do cp -p $$HLP "$$HLP#040000"; done
	$(NULIB2) -ae DAVEX.MG.SHK help/*040000
	rm -f help/*040000
	ls -l DAVEX.MG.SHK

%.p8c: %.o
	${LD65} -t none -m $@.map -o $@ $<

%.o: %.s
	$(CA65) --include-dir $(DAVEX) -l $@.lst -o $@ $<
	mkdir -p help; FILE="$<"; $(GENHELP) $< "help/$${FILE%.*}"

.PHONY: clean
clean:
	rm -f *.o *.p8c *.lst *.map davex.mg.po DAVEX.MG.SHK help/*
	if [ -d help ]; then rmdir help; fi

