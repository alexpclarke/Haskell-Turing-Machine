# Haskell Turing Machine

This program is an example of how a Turing Machine can be implemented using a purely functional programming language such as Haskell.

## Contents

- [Background](#background)
  - [What is a Turing Machine?](#what-is-a-turing-machine)
  - [Where did the Turing Machine come from?](#where-did-the-turing-machine-come-from)
  - [How does a Turing Machine work?](#how-does-a-turing-machine-work)
- [Installation](#installation)
- [Writing your program](#writing-your-program)
- [Support](#support)
- [Contributors](#contributors)
- [License](#license)

## Background

### What is a Turing Machine?

A Turing Machine is a mathematical model of computation used to define abstract machines. With the simple set of operations, a Turing Machine is able to model any computer algorithm. It is not always the most efficient method but a this model can do anything that you could do in popular programming languages such as Java. Since it's invention, there have still not been any machines that surpass a Turing Machine in terms of what problems it can solve.

### Where did the Turing Machine come from?

![Image of Alan Turing](https://www.biography.com/.image/ar_1:1%2Cc_fill%2Ccs_srgb%2Cg_face%2Cq_auto:good%2Cw_300/MTE5NDg0MDU1MTUzMTE2Njg3/alan-turing-9512017-1-402.jpg)

The Turing Machine was invented in 1939 by Alan Turing while a student as King's College in Cambridge, England. After a compelling lecture about the "decision problem" and aware of the difficulty of such proofs in mathematics, Turing invented what he called the "automatic machine" or "a-machine". Later being called a Turing Machine, this new model greatly simplified the process for proving many aspects of machines. This invention is one of the single greatest leaps in the understanding of computational logic and laid the foundation for modern computer science.

### How does a Turing Machine work?

![Image of a Turing Machine](https://d18l82el6cdm1i.cloudfront.net/uploads/dfugTjn2WC-tm_palindrome.gif)

A Turing Machine is comprised of four main components:
1. A tape that is divided into discreet cells which can contain symbols.
2. A head that can read and write to the tape as well as move the tape one cell to the left or right.
3. A state register
4. A table that tells the machine what to do based on the contents of the state register as well as the last read symbol.

Although the tape is theoretically infinite, it will initially be set to the leftmost symbol and for each step that it takes, the machine will read the tape and pass that symbol as well as the value in the state register to the instruction table. The instruction table will then then determines whether the tape should be changed, what the current state is, and if the tape needs to me moved.

## Installation

First of all, since this is not a compiled binary, you will need the Haskell compiler in order to run this program on your machine. If you do not already have Haskell installed, you can get it here:

- [Windows](http://docs.haskellstack.org/en/stable/install_and_upgrade/#windows)
- [OSX](http://docs.haskellstack.org/en/stable/install_and_upgrade/#os-x)
- [Linux](http://docs.haskellstack.org/en/stable/install_and_upgrade/#ubuntu)

Once you have Haskell installed, you are going to need to copy this repository. To do that, you may either clone it directly from GitHub or from the command line using:

```
$ git clone https://github.com/alexpclarke/Haskell-Turing-Machine.git
```

Once the program is copied to your machine, navigate to the directory and execute it by running:

```
$ stack TuringMachine.hs
```

You will then be prompted for an input file. You may write your own or select one from the examples folder.

## Writing your program

If you would like to write your own program for this simulated Turing Machine, you may do so by creating a text file which includes the following:

1. The starting state.
2. The accept states.
3. The initial value of the tape, from left to right.
4. A list of instructions for the machine to follow. (if a nonexistent transition is called, the machine will fail)

All together, the file should look like this:

```
start: s1
accept: s2, s5
tape: a, b, b, _, a, b, b, c, _, _, c, b
# this is a comment
$ (s1,  a) = (s1, _, R)
$ (s1, b) = (s2, c, R)
$ (s2, c) = (s4, a, L)
$ (s3, a) = (s2, _, I)
```

## Support

If you have found a bug or are unable to get this program working, please submit to our [GitHub issues page](https://github.com/alexpclarke/Haskell-Turing-Machine/issues) with at detailed description of the problem and we will try to address is as soon as possible.

## Contributors

- [Alex Clarke](https://github.com/alexpclarke)
- [Gabrielle Maxwell](https://github.com/gabiiiiiii)

## License

[MIT](https://github.com/alexpclarke/Haskell-Turing-Machine/blob/master/LICENSE)
