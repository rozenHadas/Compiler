%:
	echo '(load "project/final_project.scm") (compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q project/final_project.scm

	nasm -f elf64 $(MAKECMDGOALS).s -o $(MAKECMDGOALS).o

	gcc -m64 -Wall -g $(MAKECMDGOALS).o -o $(MAKECMDGOALS)
	
clean:
	rm -f test.s test.o test