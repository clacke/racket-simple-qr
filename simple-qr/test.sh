HOME_DIR="racket-simple-qr/simple-qr"
echo "func";cd; cd $HOME_DIR/lib/func;racket func-test.rkt
echo "func/capacity";cd; cd $HOME_DIR/lib/func/capacity;racket capacity-dic-test.rkt
echo "func/capacity";cd; cd $HOME_DIR/lib/func/capacity;racket capacity-func-test.rkt
echo "func/character-count";cd; cd $HOME_DIR/lib/func/character-count;racket character-bit-width-test.rkt
echo "timing-pattern";cd; cd $HOME_DIR/lib/timing-pattern;racket timing-pattern-test.rkt
echo "alignment-pattern";cd; cd $HOME_DIR/lib/alignment-pattern;racket alignment-pattern-test.rkt
echo "data-encoding";cd; cd $HOME_DIR/lib/data-encoding;racket data-encoding-test.rkt
echo "func/code-info";cd; cd $HOME_DIR/lib/func/code-info;racket code-info-test.rkt

