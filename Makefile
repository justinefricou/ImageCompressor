##
## EPITECH PROJECT, 2021
## B-FUN-400-TLS-4-1-compressor-justine.fricou
## File description:
## Makefile
##

NAME 			= 	imageCompressor

all:
	stack build --local-bin-path $(PWD)
	cp .stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/imageCompress-exe/imageCompress-exe $(NAME)


clean:
	stack clean

fclean: clean
	rm -f imageCompressor

re: fclean all

.PHONY: all clean fclean re