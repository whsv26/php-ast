<?php

class Foo {
    public __construct(public int $a ) {
        $a = $a === 1 ? $a : 0;
    }

    puf
}

$foo = new Foo(1);
$bar = 'bar';
