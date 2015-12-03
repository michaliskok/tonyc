@echo off
if exist tony.lib del tony.lib

support\masm -mx src\stdio\puti.asm;
support\masm -mx src\stdio\geti.asm;
support\masm -mx src\stdio\putc.asm;
support\masm -mx src\stdio\getc.asm;
support\masm -mx src\stdio\putb.asm;
support\masm -mx src\stdio\getb.asm;
support\masm -mx src\stdio\putr.asm;
support\masm -mx src\stdio\getr.asm;
support\masm -mx src\stdio\puts.asm;
support\masm -mx src\stdio\gets.asm;
support\masm -mx src\stdio\putbt.asm;
support\masm -mx src\stdio\getbt.asm;

support\lib tony.lib /NOIGNORECASE +puti.obj +geti.obj;
support\lib tony.lib /NOIGNORECASE +putc.obj +getc.obj;
support\lib tony.lib /NOIGNORECASE +putb.obj +getb.obj;
support\lib tony.lib /NOIGNORECASE +putr.obj +getr.obj;
support\lib tony.lib /NOIGNORECASE +puts.obj +gets.obj;
support\lib tony.lib /NOIGNORECASE +putbt.obj +getbt.obj;

support\masm -mx src\math\abs.asm;
support\masm -mx src\math\fabs.asm;
support\masm -mx src\math\sqrt.asm;
support\masm -mx src\math\sin.asm;
support\masm -mx src\math\cos.asm;
support\masm -mx src\math\tan.asm;
support\masm -mx src\math\atan.asm;
support\masm -mx src\math\exp.asm;
support\masm -mx src\math\ln.asm;
support\masm -mx src\math\pi.asm;

support\lib tony.lib /NOIGNORECASE +abs.obj +fabs.obj +sqrt.obj;
support\lib tony.lib /NOIGNORECASE +sin.obj +cos.obj +tan.obj +atan.obj;
support\lib tony.lib /NOIGNORECASE +exp.obj +ln.obj +pi.obj;

support\masm -mx src\stdlib\trunc.asm;
support\masm -mx src\stdlib\round.asm;
support\masm -mx src\stdlib\ord.asm;
support\masm -mx src\stdlib\chr.asm;
support\masm -mx src\stdlib\exit.asm;
support\masm -mx src\stdlib\shrink.asm;
support\masm -mx src\stdlib\extend.asm;

support\lib tony.lib /NOIGNORECASE +trunc.obj +round.obj;
support\lib tony.lib /NOIGNORECASE +ord.obj +chr.obj;
support\lib tony.lib /NOIGNORECASE +exit.obj;
support\lib tony.lib /NOIGNORECASE +shrink.obj +extend.obj;

support\masm -mx src\string\strlen.asm;
support\masm -mx src\string\strcmp.asm;
support\masm -mx src\string\strcpy.asm;
support\masm -mx src\string\strcat.asm;

support\lib tony.lib /NOIGNORECASE +strlen.obj +strcmp.obj;
support\lib tony.lib /NOIGNORECASE +strcpy.obj +strcat.obj;

support\masm -mx src\auxil\formati.asm;
support\masm -mx src\auxil\formatr.asm;
support\masm -mx src\auxil\parsei.asm;
support\masm -mx src\auxil\parser.asm;

support\lib tony.lib /NOIGNORECASE +formati.obj +parsei.obj;
support\lib tony.lib /NOIGNORECASE +formatr.obj +parser.obj;

support\masm -mx src\memory\gc.asm;
support\masm -mx src\memory\consv.asm;
support\masm -mx src\memory\consp.asm;
support\masm -mx src\memory\head.asm;
support\masm -mx src\memory\tail.asm;
support\masm -mx src\memory\newarrv.asm;
support\masm -mx src\memory\newarrp.asm;
support\lib tony.lib /NOIGNORECASE +gc.obj;
support\lib tony.lib /NOIGNORECASE +consv.obj +consp.obj;
support\lib tony.lib /NOIGNORECASE +head.obj +tail.obj;
support\lib tony.lib /NOIGNORECASE +newarrv.obj +newarrp.obj;

support\lib tony.lib /NOIGNORECASE, tony.lst;
ren tony.lib tony.lib
ren tony.lst tony.lst

del *.obj
del *.bak
