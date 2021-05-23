<?php

$tokens = token_get_all(file_get_contents('/home/whsv26/Documents/Projects/php-ast/src/main/php/stub.php'));

foreach ($tokens as &$token) {
    if (!is_array($token)) {
        continue;
    }

    $token[0] = token_name($token[0]);
}

print_r(json_encode($tokens));
