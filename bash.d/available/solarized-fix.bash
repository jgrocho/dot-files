# The solarized 256color dircolors has to approximate some colors for
# better compatibility. This obscure line sets those codes to the exact
# values.
case $TERM in
  *256color*)
    printf '\x1B]4;234;rgb:00/2b/36;235;rgb:07/36/42;240;rgb:58/6e/75;241;rgb:65/7b/83;244;rgb:83/94/96;245;rgb:93/a1/a1;254;rgb:ee/e8/d5;230;rgb:fd/f6/e3;136;rgb:b5/89/00;166;rgb:cb/4b/16;160;rgb:dc/32/2f;125;rgb:d3/36/82;61;rgb:6c/71/c4;33;rgb:26/8b/d2;37;rgb:2a/a1/98;64;rgb:85/99/00\a'
    ;;
esac
