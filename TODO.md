  * 2021-06-25: symbolic interpreter: notify when a Left is replaced by a Right!
    (this certainly happens in my D2 program, at pos 0)
  * 2021-06-25: I've solved day 5, even though I didn't implement correct
    instruction pointer behaviour. I'm pretty certain all I have to do is look
    at the write location before writing in `opBinOp`, but I didn't? And it
    still worked? Like, the instruction pointer can't be modified? Hrm. Maybe I
    misunderstood the specification.
