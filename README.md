I did the word filter extra credit but for fun I did it a little differently.
Instead of hard coding a file and reading from that, if you pass a string as the 4th args it will split it into
an array by spaces and then use that as the list of words to ignore. For example sbt "run 25 1 10 "none of these please""
would ignore the words "none" "of" "these" and "please"